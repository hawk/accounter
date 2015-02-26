%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter_csv).
-export([
         get_field_delim/1,
         csv_style/1,
         import_book/2,
         to_tokens/2,
         tokens_to_vouchers/3, tokens_to_items/3,
         book_to_chars/4,
         export_book/5, export_voucher/3
        ]).

-include("../include/accounter.hrl").

get_field_delim(Args) ->
    case yaws_api:queryvar(Args, delim) of
        {ok, [Delim | _]} ->
            Delim;
        _ ->
            $;
    end.

csv_style(BooksDir) ->
    File = filename:join([BooksDir, filename(new_style, account_type)]),
    case filelib:is_regular(File) of
        true  -> new_style;
        false -> old_style
    end.

filetypes() ->
    [account_type, account, budget, voucher, item].

filename(new_style, FileType) ->
    case FileType of
        account_type -> "account_types.csv";
        account      -> "accounts.csv";
        budget       -> "budget.csv";
        voucher      -> "vouchers.csv";
        item         -> "items.csv";
        error        -> "errors.txt"
    end;
filename(old_style, FileType) ->
    case FileType of
        account_type -> "Kontotyper.txt";
        account      -> "Kontotabell.txt";
        budget       -> "Budget.txt";
        voucher      -> "Verifikationer.txt";
        item         -> "Kontering.txt";
        error        -> "Felmeddelanden.txt"
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
    CsvStyle = csv_style(BooksDir),
    Files = [filename(CsvStyle, FileType) || FileType <- filetypes()],
    AllChars = lists:map(Fun, Files),
    _Errors =  [E || {_C, E} <- AllChars],
    [TChars, AChars, BChars, VChars, IChars] =  [C || {C, _E} <- AllChars],
    chars_to_book(CsvStyle, Name, TChars, AChars, BChars,
                  VChars, IChars, Delim).

file_to_chars(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} ->
            {ok, binary_to_list(Bin)};
        {error, Reason} ->
            {error,  file:format_error(Reason)}
    end.

chars_to_book(CsvStyle, Name,
              TypeChars, AccountChars, BudgetChars, VoucherChars, ItemChars,
              Delim) ->
    Types    = tokens_to_types(CsvStyle, to_tokens(TypeChars, Delim), []),
    Accounts = tokens_to_accounts(CsvStyle, to_tokens(AccountChars, Delim), []),
    Budgets  = tokens_to_budgets(CsvStyle, to_tokens(BudgetChars, Delim), []),
    Vouchers = tokens_to_vouchers(CsvStyle, to_tokens(VoucherChars, Delim), []),
    Items    = tokens_to_items(CsvStyle, to_tokens(ItemChars, Delim), []),
    accounter_check:adapt_book(Name, Types, Accounts, Budgets, Vouchers, Items).

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

tokens_to_accounts(old_style = CsvStyle,
                   [["Konto_Nr", "Konto_namn", "Konto_Typ", "K_beskrivning",
                     "Gamla konto_Nr", "resultat", "balans"] | Tail],
                   Accounts) ->
    tokens_to_accounts(CsvStyle, Tail, Accounts);
tokens_to_accounts(new_style = CsvStyle,
                   [["Id", "Name", "Type", "Description",
                     "Result", "Balance"] | Tail],
                   Accounts) ->
    tokens_to_accounts(CsvStyle, Tail, Accounts);
tokens_to_accounts(old_style = CsvStyle,
                   [[Id, Name, Type, Desc, OldId, Result, Balance] | Tail],
                   Accounts) ->
    A = (catch #account{id      = to_int(account, Id, Id),
                        name    = to_string(account, Id, Name),
                        type    = to_string(account, Id, Type),
                        desc    = to_string(account, Id, Desc),
                        old_id  = to_int(account, Id, OldId),
                        result  = to_bool(account, Id, Result),
                        balance = to_bool(account, Id, Balance)}),
    tokens_to_accounts(CsvStyle, Tail, [A | Accounts]);
tokens_to_accounts(new_style = CsvStyle,
                   [[Id, Name, Type, Desc, Result, Balance] | Tail],
                   Accounts) ->
    A = (catch #account{id      = to_int(account, Id, Id),
                        name    = to_string(account, Id, Name),
                        type    = to_string(account, Id, Type),
                        desc    = to_string(account, Id, Desc),
                        old_id  = to_int(account, Id, Id), % Backwards compat
                        result  = to_bool(account, Id, Result),
                        balance = to_bool(account, Id, Balance)}),
    tokens_to_accounts(CsvStyle, Tail, [A | Accounts]);
tokens_to_accounts(old_style, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(account, Id, Arity, "bad arity, 7 expected", ?FILE, ?LINE);
tokens_to_accounts(new_style, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(account, Id, Arity, "bad arity, 6 expected", ?FILE, ?LINE);
tokens_to_accounts(_CsvStyle, [], Accounts) ->
    lists:reverse(Accounts).

tokens_to_items(old_style = CsvStyle,
                [["Verifikations_ID", "Konto_Nr", "Debet", "Kredit",
                  "Kommentar"] | Tail], Items) ->
    tokens_to_items(CsvStyle, Tail, Items);
tokens_to_items(new_style = CsvStyle,
                [["VoucherId", "AccountId", "Debit", "Credit",
                  "Remark"] | Tail], Items) ->
    tokens_to_items(CsvStyle, Tail, Items);
tokens_to_items(CsvStyle, [[Vid, Aid, Debit, Credit, Remark] | Tail], Items) ->
    I = #item{voucher_id = to_int(item, Vid, Vid),
              account_id = to_int(item, Vid, Aid),
              amount     = {to_ore(item, Vid, Debit),
                            to_ore(item, Vid, Credit)},
              remark     = to_string(item, Vid, Remark)},
    tokens_to_items(CsvStyle, Tail, [I | Items]);
tokens_to_items(_CsvStyle, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(item, Id, Arity, "bad arity, 5 expected", ?FILE, ?LINE);
tokens_to_items(_CsvStyle, [], Items) ->
    lists:reverse(Items).

tokens_to_vouchers(old_style = CsvStyle,
                   [["Verifikations_ID", "V_Datum", "V_Text"] | Tail],
                   Vouchers) ->
    tokens_to_vouchers(CsvStyle, Tail, Vouchers);
tokens_to_vouchers(new_style = CsvStyle,
                   [["Id", "Date", "Text"] | Tail],
                   Vouchers) ->
    tokens_to_vouchers(CsvStyle, Tail, Vouchers);
tokens_to_vouchers(CsvStyle, [[Id, Date, Text] | Tail], Vouchers) ->
    V = (catch #voucher{id   = to_int(voucher, Id, Id),
                        date = to_date(voucher, Id, Date),
                        text = to_string(voucher, Id, Text)}),
    tokens_to_vouchers(CsvStyle, Tail, [V | Vouchers]);
tokens_to_vouchers(_CsvStyle, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(voucher, Id, Arity, "bad arity, 3 expected", ?FILE, ?LINE);
tokens_to_vouchers(_CsvStyle, [], Vouchers) ->
    lists:reverse(Vouchers).

tokens_to_budgets(old_style = CsvStyle,
                  [["Konto_Nr", "Konto_saldo"] | Tail],  Budgets) ->
    tokens_to_budgets(CsvStyle, Tail, Budgets);
tokens_to_budgets(new_style = CsvStyle,
                  [["AccountId", "AccountBalance"] | Tail],  Budgets) ->
    tokens_to_budgets(CsvStyle, Tail, Budgets);
tokens_to_budgets(CsvStyle, [[Id, Balance] | Tail],  Budgets) ->
    X = (catch #budget{account_id      = to_int(budget, Id, Id),
                       account_balance = to_ore(budget, Id, Balance)}),
    tokens_to_budgets(CsvStyle, Tail, [X | Budgets]);
tokens_to_budgets(_CsvStyle, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(budget, Id, Arity, "bad arity, 2 expected", ?FILE, ?LINE);
tokens_to_budgets(_CsvStyle, [], Budgets) ->
    lists:reverse(Budgets).

tokens_to_types(old_style = CsvStyle,
                [["Konto_typ", "Konto_negativ"] | Tail],  Types) ->
    tokens_to_types(CsvStyle, Tail, Types);
tokens_to_types(new_style = CsvStyle,
                [["AcountType", "Negate"] | Tail],  Types) ->
    tokens_to_types(CsvStyle, Tail, Types);
tokens_to_types(CsvStyle, [[Id, Balance] | Tail],  Types) ->
    X = (catch #account_type{name   = to_string(type, Id, Id),
                             negate = to_bool(type, Id, Balance)}),
    tokens_to_types(CsvStyle, Tail, [X | Types]);
tokens_to_types(_CsvStyle, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(type, Id, Arity, "bad arity, 2 expected", ?FILE, ?LINE);
tokens_to_types(_CsvStyle, [], Types) ->
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
    %%             [Reason, {value, Chars}, {type, Type}, {id, Id},
    %%              {file, File}, {line, Line}]}]),
    throw(#error{type = Type, id = Id, value = Chars,reason = Reason,
                 file = File, line = Line}).

%%-------------------------------------------------------------------
%% Export
%%-------------------------------------------------------------------

export_voucher(BooksDir, V, Delim) when is_record(V, voucher), is_list(Delim) ->
    CsvStyle = csv_style(BooksDir),
    VoucherChars = vouchers_to_chars(CsvStyle, [V], hd(Delim)),
    ItemChars =  voucher_items_to_chars(CsvStyle, [V], hd(Delim)),
    CsvCtyle = csv_style(BooksDir),
    VoucherFile = filename:join([BooksDir, filename(CsvCtyle, voucher)]),
    ok = file:write_file(VoucherFile, list_to_binary(VoucherChars), [append]),
    ItemFile = filename:join([BooksDir, filename(CsvCtyle, item)]),
    ok = file:write_file(ItemFile, list_to_binary(ItemChars), [append]),
    ok,
    ItemChars.

export_book(CsvStyle, Bindings, BooksDir, Book, Delim) ->
    {TypeChars, AccountChars, BudgetChars,
     VoucherChars, ItemChars, ErrorChars} =
        book_to_chars(CsvStyle, Bindings, Book, Delim),
    Dir = filename:join([BooksDir, Book#book.name]),
    TypeFile = filename:join([Dir, filename(CsvStyle, account_type)]),
    ok = file:write_file(TypeFile, list_to_binary(TypeChars)),
    AccountFile = filename:join([Dir, filename(CsvStyle, account)]),
    ok = file:write_file(AccountFile, list_to_binary(AccountChars)),
    BudgetFile = filename:join([Dir, filename(CsvStyle, budget)]),
    ok = file:write_file(BudgetFile, list_to_binary(BudgetChars)),
    VoucherFile = filename:join([Dir, filename(CsvStyle, voucher)]),
    ok = file:write_file(VoucherFile, list_to_binary(VoucherChars)),
    ItemFile = filename:join([Dir, filename(CsvStyle, item)]),
    ok = file:write_file(ItemFile, list_to_binary(ItemChars)),
    ErrorFile = filename:join([Dir, filename(CsvStyle, error)]),
    ok = file:write_file(ErrorFile, list_to_binary(ErrorChars)),
    {ok, Dir}.

book_to_chars(CsvStyle, Bindings, B, Delim) ->
    {
      [types_header(CsvStyle, Delim),
       types_to_chars(CsvStyle, B#book.types, Delim)],
      [accounts_header(CsvStyle, Delim),
       accounts_to_chars(CsvStyle, B#book.accounts, Delim)],
      [budgets_header(CsvStyle, Delim),
       budgets_to_chars(CsvStyle, B#book.accounts, Delim)],
      [vouchers_header(CsvStyle, Delim),
       vouchers_to_chars(CsvStyle, B#book.vouchers, Delim)],
      [items_header(CsvStyle, Delim),
       voucher_items_to_chars(CsvStyle, B#book.vouchers, Delim)],
      [errors_header(CsvStyle, Delim),
       errors_to_chars(CsvStyle, Bindings, B#book.errors, Delim)]
    }.

accounts_to_chars(old_style, Accounts, Delim) ->
    [
     [
      from_int(Id), Delim,
      from_string(Name), Delim,
      from_string(Type), Delim,
      from_string(Desc), Delim,
      from_int(OldId), Delim,
      from_bool(Result), Delim,
      from_bool(Balance), $\n
     ] || #account{id = Id,
                   name = Name,
                   type = Type,
                   desc = Desc,
                   old_id = OldId,
                   result = Result,
                   balance = Balance} <- Accounts].

accounts_header(old_style, Delim) ->
    [
     from_string("Konto_Nr"), Delim,
     from_string("Konto_namn"), Delim,
     from_string("Konto_Typ"), Delim,
     from_string("K_beskrivning"), Delim,
     from_string("Gamla konto_Nr"), Delim,
     from_string("resultat"), Delim,
     from_string("balans"), $\n
    ].

vouchers_to_chars(old_style, Vouchers, Delim) ->
    [
     [
      from_int(Id), Delim,
      from_date(Date), Delim,
      from_string(Text), $\n
     ] || #voucher{id   = Id,
                   date = Date,
                   text = Text} <- Vouchers].

vouchers_header(old_style, Delim) ->
    [
     from_string("Verifikations_ID"), Delim,
     from_string("V_Datum"), Delim,
     from_string("V_Text"), $\n
    ].

voucher_items_to_chars(CsvStyle, Vouchers, Delim) ->
    [items_to_chars(CsvStyle, V#voucher.items, Delim) || V <- Vouchers].

items_to_chars(old_style, Items, Delim) ->
    [
     [
      from_int(Vid), Delim,
      from_int(Aid), Delim,
      accounter:from_ore(Ore, Delim), Delim,
      from_string(Remark), $\n
     ] || #item{voucher_id = Vid,
                account_id = Aid,
                amount     = Ore,
                remark     = Remark} <- Items].

items_header(old_style, Delim) ->
    [
     from_string("Verifikations_ID"), Delim,
     from_string("Konto_Nr"), Delim,
     from_string("Debet"), Delim,
     from_string("Kredit"), Delim,
     from_string("Kommentar"), $\n
    ].

budgets_to_chars(old_style, Accounts, Delim) ->
    [
     [from_int(Id), Delim,
      from_int(Bal), $\n
     ] || #account{id     = Id,
                   budget = Bal} <- Accounts, Bal =/= undefined
    ].

budgets_header(old_style, Delim) ->
    [
     from_string("Konto_Nr"), Delim,
     from_string("Konto_saldo"), $\n
    ].

types_to_chars(old_style, Types, Delim) ->
    [
     [from_string(Name), Delim,
      from_bool(Neg), $\n
     ] || #account_type{name    = Name,
                        negate  = Neg} <- Types
    ].

types_header(old_style, Delim) ->
    [
     from_string("Konto_typ"), Delim,
     from_string("Konto_negativ"), $\n
    ].

errors_to_chars(old_style, Bindings, Errors, Delim) ->
    [
     [
      ?BINDING(string:to_upper(atom_to_list(Type)), Bindings), Delim,
      from_any(Id), Delim,
      from_any(Val), Delim,
      from_string(Reason), $\n
     ] || #error{type   = Type,
                 id     = Id,
                 value  = Val,
                 reason = Reason} <- Errors].

errors_header(old_style, Delim) ->
    [
     from_string("Typ"), Delim,
     from_string("Id"), Delim,
     from_string("Ajabaja"), Delim,
     from_string("Beskrivning"), $\n
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
