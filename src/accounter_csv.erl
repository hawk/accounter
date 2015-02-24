%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter_csv).
-export([
         get_field_delim/1,
         import_book/2,
         to_tokens/2,
         tokens_to_vouchers/2, tokens_to_items/2,
         vouchers_to_chars/2, voucher_items_to_chars/2,
         book_to_chars/3
        ]).

-include("../include/accounter.hrl").

get_field_delim(Args) ->
    case yaws_api:queryvar(Args, delim) of
        {ok, [Delim | _]} ->
            Delim;
        _ ->
            $;
    end.

%%-------------------------------------------------------------------
%% Import
%%-------------------------------------------------------------------

import_book(Args, Name) ->
    BooksDir = accounter:get_books_dir(Args),
    Delim = get_field_delim(Args),
    import_book(BooksDir, Name, Delim).

import_book(BooksDir, Name, Delim) ->
    Fun =
        fun(RelName) ->
                AbsName = filename:join([BooksDir, Name, RelName]),
                case file_to_chars(AbsName) of
                    {ok, Chars} ->
                        {Chars, []};
                    {error, Reason} ->
                        {[], [#error{type   = file,
                                     id     = filename:dirname(AbsName),
                                     value  = filename:basename(AbsName),
                                     reason = Reason,
                                     file   = ?FILE,
                                     line   = ?LINE}]}
                end
        end,
    Files = ["Kontotabell.txt",
             "Verifikationer.txt",
             "Kontering.txt",
             "Budget.txt",
             "Kontotyper.txt"],
    AllChars = lists:map(Fun, Files),
    _Errors =  [E || {_C, E} <- AllChars],
    [AChars, VChars, IChars, BChars, TChars] =  [C || {C, _E} <- AllChars],
    chars_to_book(Name, AChars, VChars, IChars, BChars, TChars, Delim).

file_to_chars(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} ->
            {ok, binary_to_list(Bin)};
        {error, Reason} ->
            {error,  file:format_error(Reason)}
    end.

chars_to_book(Name, AccountChars, VoucherChars, ItemChars,
              BudgetChars, TypeChars, Delim) ->
    Accounts = tokens_to_accounts(to_tokens(AccountChars, Delim), []),
    Vouchers = tokens_to_vouchers(to_tokens(VoucherChars, Delim), []),
    Items    = tokens_to_items(to_tokens(ItemChars, Delim), []),
    Budgets  = tokens_to_budgets(to_tokens(BudgetChars, Delim), []),
    Types    = tokens_to_types(to_tokens(TypeChars, Delim), []),
    accounter_check:adapt_book(Name, Accounts, Vouchers, Items, Budgets, Types).

%%-------------------------------------------------------------------

to_tokens(Chars, Delim) ->
    Lines = string:tokens(Chars, "\n\r"),
    Tokens = [to_tokens(Line, Delim, [])
              || Line <- Lines],
    Tokens.

to_tokens([], _Delim, Tokens) ->
    lists:reverse(Tokens);
to_tokens([DoubleQuote = $" | Chars], Delim, Tokens) ->
    Pred = fun(Char) -> Char =/= DoubleQuote end,
    case lists:splitwith(Pred, Chars) of
        {Token, [DoubleQuote | Tail]} ->
            trim_tokens(skip_spaces(Tail), Delim, [Token | Tokens]);
        {Token, []} ->
            trim_tokens([], Delim, [Token | Tokens])
        end;
to_tokens(Chars, Delim, Tokens) ->
    Pred = fun(Char) -> Char =/= Delim end,
    {Token, Tail} = lists:splitwith(Pred, Chars),
    trim_tokens(Tail, Delim, [Token | Tokens]).

trim_tokens(Tail, Delim, Tokens) ->
    case Tail of
        [Delim | Tail2] ->
            case skip_spaces(Tail2) of
                [] ->
                    %% Empty last token
                    to_tokens([], Delim, [[] | Tokens]);
                Tail3 ->
                    to_tokens(Tail3, Delim, Tokens)
            end;
        [] ->
            %% Last token
            to_tokens([], Delim, Tokens)
    end.

skip_spaces(Chars) ->
    string:strip(Chars, left, $ ).

tokens_to_accounts([["Konto_Nr", "Konto_namn", "Konto_Typ", "K_beskrivning",
                     "Gamla konto_Nr", "resultat", "balans"] | Tail],
                   Accounts) ->
    tokens_to_accounts(Tail, Accounts);
tokens_to_accounts([[Id, Name, Type, Desc, OldId, Result, Balance] | Tail],
                   Accounts) ->
    A = (catch #account{id      = to_int(account, Id, Id),
                        name    = to_string(account, Id, Name),
                        type    = to_string(account, Id, Type),
                        desc    = to_string(account, Id, Desc),
                        old_id  = to_int(account, Id, OldId),
                        result  = to_bool(account, Id, Result),
                        balance = to_bool(account, Id, Balance)}),
    tokens_to_accounts(Tail, [A | Accounts]);
tokens_to_accounts([H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(account, Id, Arity, "bad arity, 7 expected", ?FILE, ?LINE);
tokens_to_accounts([], Accounts) ->
    lists:reverse(Accounts).

tokens_to_items([["Verifikations_ID", "Konto_Nr", "Debet", "Kredit",
                  "Kommentar"] | Tail], Items) ->
    tokens_to_items(Tail, Items);
tokens_to_items([[Vid, Aid, Debit, Credit, Remark] | Tail], Items) ->
    I = #item{voucher_id = to_int(item, Vid, Vid),
              account_id = to_int(item, Vid, Aid),
              amount     = {to_ore(item, Vid, Debit),
                            to_ore(item, Vid, Credit)},
              remark     = to_string(item, Vid, Remark)},
    tokens_to_items(Tail, [I | Items]);
tokens_to_items([H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(item, Id, Arity, "bad arity, 5 expected", ?FILE, ?LINE);
tokens_to_items([], Items) ->
    lists:reverse(Items).

tokens_to_vouchers([["Verifikations_ID", "V_Datum", "V_Text"] | Tail],
                   Vouchers) ->
    tokens_to_vouchers(Tail, Vouchers);
tokens_to_vouchers([[Id, Date, Text] | Tail], Vouchers) ->
    V = (catch #voucher{id   = to_int(voucher, Id, Id),
                        date = to_date(voucher, Id, Date),
                        text = to_string(voucher, Id, Text)}),
    tokens_to_vouchers(Tail, [V | Vouchers]);
tokens_to_vouchers([H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(voucher, Id, Arity, "bad arity, 3 expected", ?FILE, ?LINE);
tokens_to_vouchers([], Vouchers) ->
    lists:reverse(Vouchers).

tokens_to_budgets([["Konto_Nr", "Konto_saldo"] | Tail],  Budgets) ->
    tokens_to_budgets(Tail, Budgets);
tokens_to_budgets([[Id, Balance] | Tail],  Budgets) ->
    X = (catch #budget{account_id      = to_int(budget, Id, Id),
                       account_balance = to_ore(budget, Id, Balance)}),
    tokens_to_budgets(Tail, [X | Budgets]);
tokens_to_budgets([H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(budget, Id, Arity, "bad arity, 2 expected", ?FILE, ?LINE);
tokens_to_budgets([], Budgets) ->
    lists:reverse(Budgets).

tokens_to_types([["Konto_typ", "Konto_negativ"] | Tail],  Types) ->
    tokens_to_types(Tail, Types);
tokens_to_types([[Id, Balance] | Tail],  Types) ->
    X = (catch #account_type{name   = to_string(type, Id, Id),
                             negate = to_bool(type, Id, Balance)}),
    tokens_to_types(Tail, [X | Types]);
tokens_to_types([H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(type, Id, Arity, "bad arity, 2 expected", ?FILE, ?LINE);
tokens_to_types([], Types) ->
    lists:reverse(Types).

to_int(Type, Id, Chars) ->
    case catch list_to_integer(Chars) of
        {'EXIT', _} ->
            bail_out(Type, Id, Chars, "bad integer", ?FILE, ?LINE);
        Int ->
            Int
    end.

to_bool(Type, Id, Chars) ->
    case Chars of
        [$0] -> false;
        [$1] -> true;
        _    -> bail_out(Type, Id, Chars, "bad boolean", ?FILE, ?LINE)
    end.

to_string(_Type, _Id, Chars) ->
    Chars. %string:strip(Chars, both, $").

to_ore(Type, Id, Chars) ->
    case string:tokens(Chars, ", ") of
        [Kr, Ore, "kr"] ->
            100 * to_int(Type, Id, strip(Kr)) + to_int(Type, Id, strip(Ore));
        [Kr, "kr"] ->
            100 * to_int(Type, Id, strip(Kr));
        [Kr, Ore] ->
            100 * to_int(Type, Id, strip(Kr)) + to_int(Type, Id, strip(Ore));
        [Kr] ->
            100 * to_int(Type, Id, strip(Kr));
        _A->
            bail_out(Type, Id, Chars, "bad amount, should be like 123,45 kr",
                  ?FILE, ?LINE)
    end.

strip(String) ->
    string:strip(String, both, $ ).

to_date(Type, Id, Chars) ->
    case string:tokens(Chars, "-: ") of
        [Year, Month, Day | _HourMinSec] ->
            {to_int(Type, Id, Year),
             to_int(Type, Id, Month),
             to_int(Type, Id, Day)};
        _ ->
            bail_out(Type, Id, Chars,
                  "bad date, should be like YYYY-MM-DD HH:MM:SS", ?FILE, ?LINE)
    end.

bail_out(Type, Id, Chars, Reason, File, Line) ->
    %% io:format("ERROR: ~p ~p\n",
    %%           [list_to_tuple(Chars),
    %%            {error,
    %%             [Reason, {value, Chars}, {type, Type}, {id, Id}, {file, File}, {line, Line}]}]),
    throw(#error{type = Type, id = Id, value = Chars,reason = Reason, file = File, line = Line}).

%%-------------------------------------------------------------------
%% Export
%%-------------------------------------------------------------------

%% export_book(BooksDir, Book, Delim) ->
%%     {AccountChars, VoucherChars, ItemChars,
%%      BudgetChars, TypeChars, ErrorChars} =
%%         book_to_chars(Book, Delim),
%%     Dir = filename:join([BooksDir, Book#book.name]),
%%     AccountFile = filename:join([Dir, "Kontotabell.txt"]),
%%     ok = file:write_file(AccountFile, list_to_binary(AccountChars)),
%%     VoucherFile = filename:join([Dir, "Verifikationer.txt"]),
%%     ok = file:write_file(VoucherFile, list_to_binary(VoucherChars)),
%%     ItemFile = filename:join([Dir, "Kontering.txt"]),
%%     ok = file:write_file(ItemFile, list_to_binary(ItemChars)),
%%     BudgetFile = filename:join([Dir, "Budget.txt"]),
%%     ok = file:write_file(BudgetFile, list_to_binary(BudgetChars)),
%%     TypeFile = filename:join([Dir, "Kontotyper.txt"]),
%%     ok = file:write_file(TypeFile, list_to_binary(TypeChars)),
%%     ErrorFile = filename:join([Dir, "Felmeddelanden.txt"]),
%%     ok = file:write_file(ErrorFile, list_to_binary(ErrorChars)),
%%     {ok, Dir}.

book_to_chars(Bindings, B, Delim) ->
    {[accounts_header(Delim), accounts_to_chars(B#book.accounts, Delim)],
     [vouchers_header(Delim), vouchers_to_chars(B#book.vouchers, Delim)],
     [items_header(Delim),    voucher_items_to_chars(B#book.vouchers, Delim)],
     [budgets_header(Delim),  budgets_to_chars(B#book.accounts, Delim)],
     [types_header(Delim),    types_to_chars(B#book.types, Delim)],
     [errors_header(Delim),   errors_to_chars(Bindings, B#book.errors, Delim)]
    }.

accounts_to_chars(Accounts, Delim) ->
    [
     [
      from_int(Id), Delim,
      from_string(Name), Delim,
      from_string(Type), Delim,
      from_string(Desc), Delim,
      from_int(OldId), Delim,
      from_bool(Result), Delim,
      from_bool(Balance), $\r, $\n
     ] || #account{id = Id,
                   name = Name,
                   type = Type,
                   desc = Desc,
                   old_id = OldId,
                   result = Result,
                   balance = Balance} <- Accounts].

accounts_header(Delim) ->
    [
     from_string("Konto_Nr"), Delim,
     from_string("Konto_namn"), Delim,
     from_string("Konto_Typ"), Delim,
     from_string("K_beskrivning"), Delim,
     from_string("Gamla konto_Nr"), Delim,
     from_string("resultat"), Delim,
     from_string("balans"), $\r, $\n
    ].

vouchers_to_chars(Vouchers, Delim) ->
    [
     [
      from_int(Id), Delim,
      from_date(Date), Delim,
      from_string(Text), $\r, $\n
     ] || #voucher{id   = Id,
                   date = Date,
                   text = Text} <- Vouchers].

vouchers_header(Delim) ->
    [
     from_string("Verifikations_ID"), Delim,
     from_string("V_Datum"), Delim,
     from_string("V_Text"), $\r, $\n
    ].

voucher_items_to_chars(Vouchers, Delim) ->
    [items_to_chars(V#voucher.items, Delim) || V <- Vouchers].

items_to_chars(Items, Delim) ->
    [
     [
      from_int(Vid), Delim,
      from_int(Aid), Delim,
      accounter:from_ore(Ore, Delim), Delim,
      from_string(Remark), $\r, $\n
     ] || #item{voucher_id = Vid,
                account_id = Aid,
                amount     = Ore,
                remark     = Remark} <- Items].

items_header(Delim) ->
    [
     from_string("Verifikations_ID"), Delim,
     from_string("Konto_Nr"), Delim,
     from_string("Debet"), Delim,
     from_string("Kredit"), Delim,
     from_string("Kommentar"), $\r, $\n
    ].

budgets_to_chars(Accounts, Delim) ->
    [
     [from_int(Id), Delim,
      from_int(Bal), $\r, $\n
     ] || #account{id     = Id,
                   budget = Bal} <- Accounts, Bal =/= undefined
    ].

budgets_header(Delim) ->
    [
     from_string("Konto_Nr"), Delim,
     from_string("Konto_saldo"), $\r, $\n
    ].

types_to_chars(Types, Delim) ->
    [
     [from_string(Name), Delim,
      from_bool(Neg), $\r, $\n
     ] || #account_type{name    = Name,
                        negate  = Neg} <- Types
    ].

types_header(Delim) ->
    [
     from_string("Konto_typ"), Delim,
     from_string("Konto_negativ"), $\r, $\n
    ].

errors_to_chars(Bindings, Errors, Delim) ->
    [
     [
      ?BINDING(string:to_upper(atom_to_list(Type)), Bindings), Delim,
      from_any(Id), Delim,
      from_any(Val), Delim,
      from_string(Reason), $\r, $\n
     ] || #error{type   = Type,
                 id     = Id,
                 value  = Val,
                 reason = Reason} <- Errors].

errors_header(Delim) ->
    [
     from_string("Typ"), Delim,
     from_string("Id"), Delim,
     from_string("Ajabaja"), Delim,
     from_string("Beskrivning"), $\r, $\n
    ].

from_any(Int) when is_integer(Int) ->
    from_int(Int);
from_any(Date = {_, _, _}) ->
    from_date(Date);
from_any(String) when is_list(String) ->
    from_string(String).

from_int(Int) ->
    integer_to_list(Int).

from_string([]) ->
    [];
from_string(String) ->
    [$", String, $"].

from_date({Year, Month, Day}) ->
    [
     from_int(Year),
     $-,
     from_int(Month),
     $-,
     from_int(Day)
     | " 00:00:00"
    ].

from_bool(Bool) ->
    case Bool of
        false -> $0;
        true  -> $1
    end.
