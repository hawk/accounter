%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter_csv).
-export([
         get_field_delim/1,
         csv_style/1,
         filename/2,
         import_book/2,
         to_tokens/2,
         tokens_to_vouchers/3, tokens_to_items/3,
         encode_book/4, export_book/5, export_voucher/4
        ]).

-include("../include/accounter.hrl").

get_field_delim(Args) ->
    case yaws_api:queryvar(Args, delim) of
        {ok, [Delim | _]} ->
            Delim;
        _ ->
            $;
    end.

csv_style(YearDir) ->
    NewFile = filename:join([YearDir, filename(new_style, budget)]),
    OldFile = filename:join([YearDir, filename(old_style, budget)]),
    case {filelib:is_regular(NewFile), filelib:is_regular(OldFile)} of
        {true, _}      -> new_style;
        {_, true}      -> old_style
        {false, false} -> newer_style
    end.

filetypes(newer_style) ->
    [account_type, account, voucher, item];
filetypes(_CsvStyle) ->
    [account_type, account, budget, voucher, item].

filename(newer_style, FileType) ->
    case FileType of
        account_type -> "account_types.csv";
        account      -> "accounts.csv";
        voucher      -> "vouchers.csv";
        item         -> "items.csv";
        error        -> "errors.csv"
    end;
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
    YearDir = filename:join([BooksDir, Name]),
    Fun =
        fun(RelName) ->
                AbsName = filename:join([YearDir, RelName]),
                case scan_file(AbsName) of
                    {ok, IoList} ->
                        {IoList, []};
                    {error, Reason} ->
                        {[], [#error{type   = file,
                                     id     = filename:dirname(AbsName),
                                     value  = filename:basename(AbsName),
                                     reason = Reason,
                                     file   = ?FILE,
                                     line   = ?LINE}]}
                end
        end,
    CsvStyle = csv_style(YearDir),
    Files = [filename(CsvStyle, FileType) || FileType <- filetypes(CsvStyle)],
    AllIoList = lists:map(Fun, Files),
    [TIoList, AIoList, BIoList, VIoList, IIoList] =
        [C || {C, _E} <- AllIoList],
    decode_book(CsvStyle, Name, TIoList, AIoList, BIoList,
                  VIoList, IIoList, Delim).

scan_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} ->
            {ok, binary_to_list(Bin)};
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

decode_book(CsvStyle, Name,
            TypeIoList, AccountIoList, BudgetIoList, VoucherIoList, ItemIoList,
            Delim) ->
    Types = tokens_to_types(CsvStyle,
                            to_tokens(TypeIoList, Delim), []),
    Accounts = tokens_to_accounts(CsvStyle,
                                  to_tokens(AccountIoList, Delim), Types, []),
    Budgets = tokens_to_budgets(CsvStyle,
                                to_tokens(BudgetIoList, Delim), Accounts,[]),
    Vouchers = tokens_to_vouchers(CsvStyle,
                                  to_tokens(VoucherIoList, Delim), []),
    Items = tokens_to_items(CsvStyle,
                            to_tokens(ItemIoList, Delim), []),
    accounter_check:amend_book(Name, Types, Accounts, Budgets, Vouchers, Items).

%%-------------------------------------------------------------------

to_tokens(Chars, Delim) ->
    Lines = string:tokens(Chars, "\n\r"),
    [to_tokens(Line, Delim, []) || Line <- Lines].

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

tokens_to_types(newer_style = CsvStyle,
                [["AccountType", "Negate", "InBalance"] | Tail],
                Types) ->
    tokens_to_types(CsvStyle_style, Tail, Types);
tokens_to_types(new_style = CsvStyle,
                [["AccountType", "Negate", "InResult", "InBalance"] | Tail],
                Types) ->
    tokens_to_types(CsvStyle, Tail, Types);
tokens_to_types(old_style = CsvStyle,
                [["Konto_type", "Konto_negativ"] | Tail],  Types) ->
    tokens_to_types(CsvStyle, Tail, Types);
tokens_to_types(newer_style = CsvStyle,
                [[Id, Negate, InBalance] | Tail],
                Types) ->
    X = (catch #account_type{name   = to_string(type, Id, Id),
                             negate = to_bool(type, Id, Negate),
                             in_balance = to_bool(type, Id, InBalance)}),
    tokens_to_types(CsvStyle, Tail, [X | Types]);
tokens_to_types(new_style = CsvStyle,
                [[Id, Negate, _InResult, InBalance] | Tail],
                Types) ->
    X = (catch #account_type{name   = to_string(type, Id, Id),
                             negate = to_bool(type, Id, Negate),
                             in_balance = to_bool(type, Id, InBalance)}),
    tokens_to_types(CsvStyle, Tail, [X | Types]);
tokens_to_types(old_style = CsvStyle, [[Id, Negate] | Tail],  Types) ->
    X = (catch #account_type{name   = to_string(type, Id, Id),
                             negate = to_bool(type, Id, Negate)}),
    tokens_to_types(CsvStyle, Tail, [X | Types]);
tokens_to_types(newer_style, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(type, Id, Arity, "bad arity, 3 expected", ?FILE, ?LINE);
tokens_to_types(new_style, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(type, Id, Arity, "bad arity, 4 expected", ?FILE, ?LINE);
tokens_to_types(old_style, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(type, Id, Arity, "bad arity, 2 expected", ?FILE, ?LINE);
tokens_to_types(_CsvStyle, [], Types) ->
    lists:reverse(Types).

tokens_to_accounts(old_style = CsvStyle,
                   [["Konto_Nr", "Konto_namn", "Konto_Typ", "K_beskrivning",
                     "Gamla konto_Nr", "resultat", "balans"] | Tail],
                   Types,
                   Accounts) ->
    tokens_to_accounts(CsvStyle, Tail, Types, Accounts);
tokens_to_accounts(new_style = CsvStyle,
                   [["Id", "Name", "Type", "Description",
                     "Budget", "InResult", "InBalance"] | Tail],
                   Types,
                   Accounts) ->
    tokens_to_accounts(CsvStyle, Tail, Types, Accounts);
tokens_to_accounts(newer_style = CsvStyle,
                   [["Id", "Name", "Type", "Description",
                     "Budget", "InBalance"] | Tail],
                   Types,
                   Accounts) ->
    tokens_to_accounts(CsvStyle_style, Tail, Types, Accounts);
tokens_to_accounts(old_style = CsvStyle,
                   [[Id, Name, Type, Desc, OldId,
                     InResult, InBalance] | Tail],
                   Types,
                   Accounts) ->
    A = (catch #account{id      = to_int(account, Id, Id),
                        old_id  = to_int(account, Id, OldId),
                        name    = to_string(account, Id, Name),
                        type    = to_string(account, Id, Type),
                        desc    = to_string(account, Id, Desc),
                        in_result = to_bool(account, Id, InResult),
                        in_balance = to_bool(account, Id, InBalance)}),
    tokens_to_accounts(CsvStyle, Tail, Types, [A | Accounts]);
tokens_to_accounts(new_style = CsvStyle,
                   [[Id, Name, Type, Desc, Budget, Result, InBalance] | Tail],
                   Types,
                   Accounts) ->
    IdInt = to_int(account, Id, Id),
    A = (catch #account{id      = IdInt,
                        old_id  = IdInt, % Backwards compat
                        name    = to_string(account, IdInt, Name),
                        type    = Type,
                        desc    = to_string(account, IdInt, Desc),
                        budget  = opt_to_int(account, IdInt, Budget),
                        in_result =
                            to_bool(account, IdInt, Result, Type, Types,
                                    #account_type.in_result),
                        in_balance =
                            to_bool(account, IdInt, InBalance, Type, Types,
                                    #account_type.in_balance)}),
    tokens_to_accounts(CsvStyle, Tail, Types, [A | Accounts]);
tokens_to_accounts(newer_style = CsvStyle,
                   [[Id, Name, Type, Desc, Budget, Result, InBalance] | Tail],
                   Types,
                   Accounts) ->
    IdInt = to_int(account, Id, Id),
    A = (catch #account{id      = IdInt,
                        old_id  = IdInt, % Backwards compat
                        name    = to_string(account, IdInt, Name),
                        type    = Type,
                        desc    = to_string(account, IdInt, Desc),
                        budget  = opt_to_int(account, IdInt, Budget),
                        in_result =
                            to_bool(account, IdInt, Result, Type, Types,
                                    #account_type.in_result),
                        in_balance =
                            to_bool(account, IdInt, Balance, Type, Types,
                                    #account_type.in_balance)}),
    tokens_to_accounts(CsvStyle, Tail, Types, [A | Accounts]);
tokens_to_accounts(old_style, [H = [Id | _] | _Tail], _, _) ->
    Arity = integer_to_list(length(H)),
    bail_out(account, Id, Arity, "bad arity, 7 expected", ?FILE, ?LINE);
tokens_to_accounts(new_style, [H = [Id | _] | _Tail], _,  _) ->
    Arity = integer_to_list(length(H)),
    bail_out(account, Id, Arity, "bad arity, 7 expected", ?FILE, ?LINE);
tokens_to_accounts(newer_style, [H = [Id | _] | _Tail], _,  _) ->
    Arity = integer_to_list(length(H)),
    bail_out(account, Id, Arity, "bad arity, 6 expected", ?FILE, ?LINE);
tokens_to_accounts(_CsvStyle, [], _, Accounts) ->
    lists:reverse(Accounts).

tokens_to_budgets(new_style,
                  [],
                  Accounts,
                  []) ->
    [#budget{account_id = Id, account_balance = Budget} ||
        #account{id = Id, budget = Budget} <- Accounts];
tokens_to_budgets(old_style = CsvStyle,
                  [["Konto_Nr", "Konto_saldo"] | Tail],
                 Accounts,
                  Budgets) ->
    tokens_to_budgets(CsvStyle, Tail, Accounts, Budgets);
tokens_to_budgets(old_style = CsvStyle,
                  [[Id, Balance] | Tail], Accounts, Budgets) ->
    X = (catch #budget{account_id      = to_int(budget, Id, Id),
                       account_balance = to_ore(budget, Id, Balance)}),
    tokens_to_budgets(CsvStyle, Tail, Accounts, [X | Budgets]);
tokens_to_budgets(old_style, [H = [Id | _] | _Tail], _, _) ->
    Arity = integer_to_list(length(H)),
    bail_out(budget, Id, Arity, "bad arity, 2 expected", ?FILE, ?LINE);
tokens_to_budgets(old_style, [], _, Budgets) ->
    lists:reverse(Budgets).

tokens_to_vouchers(new_style = CsvStyle,
                   [["Id", "Date", "Text"] | Tail],
                   Vouchers) ->
    tokens_to_vouchers(CsvStyle, Tail, Vouchers);
tokens_to_vouchers(old_style = CsvStyle,
                   [["Verifikations_ID", "V_Datum", "V_Text"] | Tail],
                   Vouchers) ->
    tokens_to_vouchers(CsvStyle, Tail, Vouchers);
tokens_to_vouchers(CsvStyle, [[Id, Date, Text] | Tail], Vouchers) ->
    V = (catch #voucher{id   = to_int(voucher, Id, Id),
                        date = to_date(CsvStyle, voucher, Id, Date),
                        text = to_string(voucher, Id, Text)}),
    tokens_to_vouchers(CsvStyle, Tail, [V | Vouchers]);
tokens_to_vouchers(_CsvStyle, [H = [Id | _] | _Tail], _) ->
    Arity = integer_to_list(length(H)),
    bail_out(voucher, Id, Arity, "bad arity, 3 expected", ?FILE, ?LINE);
tokens_to_vouchers(_CsvStyle, [], Vouchers) ->
    lists:reverse(Vouchers).

tokens_to_items(new_style = CsvStyle,
                [["VoucherId", "AccountId", "Debit", "Credit",
                  "Remark"] | Tail], Items) ->
    tokens_to_items(CsvStyle, Tail, Items);
tokens_to_items(old_style = CsvStyle,
                [["Verifikations_ID", "Konto_Nr", "Debet", "Kredit",
                  "Kommentar"] | Tail], Items) ->
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

opt_to_int(_Type, _Id, "") ->
    undefined;
opt_to_int(Type, Id, Chars) ->
    to_int(Type, Id, Chars).

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

to_bool(Type, Id, Chars, AccuntType, AccountTypes, Pos) ->
    case Chars of
        [$0] -> false;
        [$1] -> true;
        []   -> add_default(AccuntType, AccountTypes, Pos, Chars);
        _    -> bail_out(Type, Id, Chars, "bad boolean", ?FILE, ?LINE)
    end.

add_default(AccountType, AccountTypes, Pos, Chars) ->
    case lists:keyfind(AccountType, #account_type.name, AccountTypes) of
        false ->
            io:format("Types ~p ~p ~p\n",
                      [AccountType, #account_type.name, AccountTypes]),
            bail_out(account_type, AccountType, Chars, "missing account type",
                     ?FILE, ?LINE);
        Default ->
            element(Pos, Default)
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

to_date(CsvStyle, Type, Id, Chars) ->
    case string:tokens(Chars, "-: ") of
        [Year, Month, Day | _HourMinSec] ->
            {to_int(Type, Id, Year),
             to_int(Type, Id, Month),
             to_int(Type, Id, Day)};
        _ when CsvStyle =:= old_style ->
            bail_out(Type, Id, Chars,
                  "bad date, should be like YYYY-MM-DD HH:MM:SS", ?FILE, ?LINE);
        _ when CsvStyle =:= new_style ->
            bail_out(Type, Id, Chars,
                  "bad date, should be like YYYY-MM-DD", ?FILE, ?LINE)
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

export_voucher(BooksDir, Book, V, Delim) when is_record(V, voucher) ->
    YearDir = filename:join([BooksDir, Book#book.name]),
    CsvStyle = csv_style(YearDir),
    case accounter:get_latest_voucher_id(Book) + 1 of
        NextFreeId when NextFreeId =:= V#voucher.id ->
            append_voucher(CsvStyle, YearDir, V, Delim);
        _ ->
            replace_voucher(CsvStyle, YearDir, Book, V, Delim)
    end.

append_voucher(CsvStyle, YearDir, V, Delim) ->
    write_files(YearDir,
                [{filename(CsvStyle, voucher),
                  encode_vouchers(CsvStyle, [V], Delim)},
                 {filename(CsvStyle, item),
                  encode_voucher_items(CsvStyle, [V], Delim)}],
                append).

replace_voucher(CsvStyle, YearDir, B, V, Delim) ->
    Vouchers = lists:keystore(V#voucher.id, #voucher.id, B#book.vouchers, V),
    VoucherIoList =
        [vouchers_header(CsvStyle, Delim),
         encode_vouchers(CsvStyle, Vouchers, Delim)],
    ItemIoList =
        [items_header(CsvStyle, Delim),
         encode_voucher_items(CsvStyle, Vouchers, Delim)],
    write_files(YearDir,
                [{filename(CsvStyle, voucher), VoucherIoList},
                 {filename(CsvStyle, item),    ItemIoList}],
                replace),
    {ok, YearDir}.

export_book(CsvStyle, Bindings, BooksDir, Book, Delim) ->
    {TypeIoList, AccountIoList, BudgetIoList,
     VoucherIoList, ItemIoList, ErrorIoList} =
        encode_book(CsvStyle, Bindings, Book, Delim),
    Dir = filename:join([BooksDir, Book#book.name]),
    write_files(Dir,
                [{filename(CsvStyle, account_type), TypeIoList},
                 {filename(CsvStyle, account),      AccountIoList},
                 {filename(CsvStyle, budget),       BudgetIoList},
                 {filename(CsvStyle, voucher),      VoucherIoList},
                 {filename(CsvStyle, item),         ItemIoList},
                 {filename(CsvStyle, error),        ErrorIoList}],
                replace),
    {ok, Dir}.

encode_book(CsvStyle, Bindings, B, Delim) ->
    {
      [types_header(CsvStyle, Delim),
       encode_types(CsvStyle, B#book.types, Delim)],
      [accounts_header(CsvStyle, Delim),
       encode_accounts(CsvStyle, B#book.accounts, B#book.types, Delim)],
      [budgets_header(CsvStyle, Delim),
       encode_budgets(CsvStyle, B#book.accounts, Delim)],
      [vouchers_header(CsvStyle, Delim),
       encode_vouchers(CsvStyle, B#book.vouchers, Delim)],
      [items_header(CsvStyle, Delim),
       encode_voucher_items(CsvStyle, B#book.vouchers, Delim)],
      [errors_header(CsvStyle, Delim),
       encode_errors(CsvStyle, Bindings, B#book.errors, Delim)]
    }.

types_header(old_style, Delim) ->
    [
     from_string("Konto_typ"), Delim,
     from_string("Konto_negativ"), $\n
    ];
types_header(new_style, Delim) ->
    [
     from_string("AccountType"), Delim,
     from_string("Negate"), Delim,
     from_string("InResult"), Delim,
     from_string("InBalance"), $\n
    ].

encode_types(old_style, Types, Delim) ->
    [
     [from_string(Name), Delim,
      from_bool(Neg), $\n
     ] || #account_type{name   = Name,
                        negate = Neg} <- Types
    ];
encode_types(new_style, Types, Delim) ->
    [
     [from_string(Name), Delim,
      from_bool(Neg), Delim,
      from_bool(InBalance), $\n
     ] || #account_type{name   = Name,
                        negate = Neg,
                        in_balance = InBalance} <- Types
    ].

accounts_header(old_style, Delim) ->
    [
     from_string("Konto_Nr"), Delim,
     from_string("Konto_namn"), Delim,
     from_string("Konto_Typ"), Delim,
     from_string("K_beskrivning"), Delim,
     from_string("Gamla konto_Nr"), Delim,
     from_string("resultat"), Delim,
     from_string("balans"), $\n
    ];
accounts_header(new_style, Delim) ->
    [
     from_string("Id"), Delim,
     from_string("Name"), Delim,
     from_string("Type"), Delim,
     from_string("Description"), Delim,
     from_string("Budget"), Delim,
     from_string("InResult"), Delim,
     from_string("InBalance"), $\n
    ].

encode_accounts(old_style, Accounts, _Types, Delim) ->
    [
     [
      from_int(Id), Delim,
      from_string(Name), Delim,
      from_string(Type), Delim,
      from_string(Desc), Delim,
      from_int(OldId), Delim,
      from_bool(InResult), Delim,
      from_bool(InBalance), $\n
     ] || #account{id = Id,
                   old_id = OldId,
                   name = Name,
                   type = Type,
                   desc = Desc,
                   in_result = InResult,
                   in_balance = InBalance} <- Accounts];
encode_accounts(new_style, Accounts, Types, Delim) ->
    [
     [
      from_int(Id), Delim,
      from_string(Name), Delim,
      from_string(Type), Delim,
      from_string(Desc), Delim,
      opt_from_int(Budget), Delim,
      opt_from_bool(InResult, Type, #account_type.in_result, Types), Delim,
      opt_from_bool(InBalance, Type, #account_type.in_balance, Types), $\n
     ] || #account{id = Id,
                   name = Name,
                   type = Type,
                   desc = Desc,
                   budget = Budget,
                   in_result = InResult,
                   in_balance = InBalance} <- Accounts].

budgets_header(old_style, Delim) ->
    [
     from_string("Konto_Nr"), Delim,
     from_string("Konto_saldo"), $\n
    ];
budgets_header(new_style, Delim) ->
    [
     from_string("AccountId"), Delim,
     from_string("AccountBalance"), $\n
    ].

encode_budgets(_CsvStyle, Accounts, Delim) ->
    [
     [from_int(Id), Delim,
      from_int(Bal), $\n
     ] || #account{id     = Id,
                   budget = Bal} <- Accounts, Bal =/= undefined
    ].

vouchers_header(old_style, Delim) ->
    [
     from_string("Verifikations_ID"), Delim,
     from_string("V_Datum"), Delim,
     from_string("V_Text"), $\n
    ];
vouchers_header(new_style, Delim) ->
    [
     from_string("Id"), Delim,
     from_string("Date"), Delim,
     from_string("Text"), $\n
    ].

encode_vouchers(CsvStyle, Vouchers, Delim) ->
    [
     [
      from_int(Id), Delim,
      from_date(CsvStyle,Date), Delim,
      from_string(Text), $\n
     ] || #voucher{id   = Id,
                   date = Date,
                   text = Text} <- Vouchers].

items_header(old_style, Delim) ->
    [
     from_string("Verifikations_ID"), Delim,
     from_string("Konto_Nr"), Delim,
     from_string("Debet"), Delim,
     from_string("Kredit"), Delim,
     from_string("Kommentar"), $\n
    ];
items_header(new_style, Delim) ->
    [
     from_string("VoucherId"), Delim,
     from_string("AccountId"), Delim,
     from_string("Debit"), Delim,
     from_string("Credit"), Delim,
     from_string("Remark"), $\n
    ].

encode_voucher_items(CsvStyle, Vouchers, Delim) ->
    [encode_items(CsvStyle, V#voucher.items, Delim) || V <- Vouchers].

encode_items(_CsvStyle, Items, Delim) ->
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

encode_errors(CsvStyle, Bindings, Errors, Delim) ->
    [
     [
      ?BINDING(string:to_upper(atom_to_list(Type)), Bindings), Delim,
      from_any(CsvStyle, Id), Delim,
      from_any(CsvStyle, Val), Delim,
      from_string(Reason), $\n
     ] || #error{type   = Type,
                 id     = Id,
                 value  = Val,
                 reason = Reason} <- Errors].

errors_header(newer_style, Delim) ->
    [
     from_string("Type"), Delim,
     from_string("Id"), Delim,
     from_string("BadValue"), Delim,
     from_string("Reason"), $\n
    ];
errors_header(new_style, Delim) ->
    [
     from_string("Type"), Delim,
     from_string("Id"), Delim,
     from_string("Bad value"), Delim,
     from_string("Reason"), $\n
    ];
errors_header(old_style, Delim) ->
    [
     from_string("Typ"), Delim,
     from_string("Id"), Delim,
     from_string("Dåligt värde"), Delim,
     from_string("Beskrivning"), $\n
    ].

from_any(_CsvStyle, Int) when is_integer(Int) ->
    from_int(Int);
from_any(CsvStyle, Date = {_, _, _}) ->
    from_date(CsvStyle, Date);
from_any(_CsvStyle, String) when is_list(String) ->
    from_string(String).

opt_from_int(undefined) ->
    "";
opt_from_int(Int) ->
    from_int(Int).

from_int(Int) ->
    integer_to_list(Int).

from_string([]) ->
    [];
from_string(String) ->
    [$", String, $"].

from_date(old_style, Date) ->
    [from_date(new_style, Date), " 00:00:00"];
from_date(new_style, {Year, Month, Day}) ->
    [
     from_int(Year),
     $-,
     from_int(Month),
     $-,
     from_int(Day)
    ].

from_bool(Bool) ->
    case Bool of
        false -> $0;
        true  -> $1
    end.

opt_from_bool(Bool, Type, Pos, Types) ->
    T = lists:keyfind(Type, #account_type.name, Types),
    Val = element(Pos, T),
    if
        Bool =:= unefined ->
            "";
        Val =:= Bool ->
            "";
        true ->
            from_bool(Bool)
    end.

write_files(Dir, [{FileBase, IoList} | T], append) ->
    File = filename:join([Dir, FileBase]),
    write_file(File, IoList, [append]),
    write_files(Dir, T, append);
write_files(Dir, [{FileBase, IoList} | T], replace) ->
    File = filename:join([Dir, FileBase]),
    TmpFile = File ++ ".tmp",
    write_file(TmpFile, IoList, []),
    write_files(Dir, T, replace),
    ok = file:rename(TmpFile, File);
write_files(_Dir, [], _Mode) ->
    ok.

write_file(File, IoList, Opts) ->
    case file:write_file(File, IoList, Opts) of
        ok ->
            ok;
        {error, Reason} ->
            ReasonStr = file:format_error(Reason),
            bail_out(file, File, IoList, ReasonStr, ?FILE, ?LINE)
    end.
