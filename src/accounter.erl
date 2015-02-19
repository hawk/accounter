%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter).
-compile([export_all]).

-include_lib("kernel/include/file.hrl").
-include("../lib/yaws/include/yaws.hrl").
-include("../lib/yaws/include/yaws_api.hrl").
-include("../include/accounter.hrl").

%%-------------------------------------------------------------------
%% Library functions
%%-------------------------------------------------------------------

get_work_dir(Args) ->
    Error = fun() -> exit(no_work_dir) end,
    get_config(Args, work_dir, Error).

get_books_dir(Args) ->
    filename:join([get_work_dir(Args), "books"]).

get_config_file(Args) ->
    filename:join([get_work_dir(Args), "config.xml"]).

get_bindings(Args) ->
    ConfigFile = get_config_file(Args),
    Config = accounter_xml:parse_simple(ConfigFile),
    FileContext = [ConfigFile, file],
    Accounter = ?XML_LOOKUP(accounter, [Config], FileContext),
    AccounterContext = [accounter | FileContext],
    Bindings = ?XML_LOOKUP(bindings, Accounter, AccounterContext),
    BindingsContext = [bindings | AccounterContext],
    [format_binding(B, BindingsContext, ?FILE, ?LINE) || B <- Bindings].

format_binding({binding, Binding}, BindingsContext, _, _) ->
    BindingContext = [binding | BindingsContext],
    Replace = ?XML_LOOKUP(replace, Binding, BindingContext),
    With = ?XML_LOOKUP(with, Binding, BindingContext),
    {Replace, With};
format_binding([], _BindingsContext, _, _) ->
    [];
format_binding(Bindings, BindingsContext, File, Line) ->
    accounter_xml:missing(binding, Bindings, BindingsContext, File, Line).

lookup_binding(Replace, Bindings, File, Line) ->
    Error =
        fun() ->
                exit({error,
                      [missing_binding,
                       Replace,
                       Bindings,
                       {file,File}, {line,Line}]})
        end,
    case lookup(Replace, Bindings, Error) of
        "" ->
            exit({error,
                  [empty_binding,
                   Replace,
                   Bindings,
                   {file,File}, {line,Line}]});
        With ->
            With
    end.

get_config(#arg{opaque = Opaque}, Key, Default) ->
    %% io:format("Opaque: ~p\n", [Opaque]),
    lookup(Key, Opaque, Default).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false when is_function(Default) ->
            Default();
        false ->
            Default;
        {Key, Val} ->
            Val
    end.

get_var(#arg{} = Args, Key, Default) ->
   case yaws_api:queryvar(Args, Key) of
        {ok, Val} ->
            Val;
        undefined when is_function(Default) ->
            Default();
        undefined ->
            Default
    end.

get_book_name(Args) ->
    Fun = fun() ->
                  BooksDir = get_books_dir(Args),
                  case list_books(BooksDir) of
                      {ok , [Name | _]} ->
                    Name;
                      {error, _Reason} ->
                          integer_to_list(element(1, erlang:date()))
                  end
          end,
    get_var(Args, name, Fun).

get_voucher_id(Args, B) ->
    get_var(Args, id,
            fun() -> lists:max([V#voucher.id || V <- B#book.vouchers]) + 1 end).

get_field_delim(Args) ->
    case yaws_api:queryvar(Args, delim) of
        {ok, [Delim | _]} ->
            Delim;
        _ ->
            $;
    end.

list_books(BooksDir) ->
    case file:list_dir(BooksDir) of
        {ok, Names} ->
            Rev = fun(X, Y) -> X > Y end,
            Names2 = [Name || Name <- lists:sort(Rev, Names),
                              is_dir(BooksDir, Name)],
            {ok, Names2};
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

is_dir(BooksDir, Name) ->
    DirName = filename:join([BooksDir, Name]),
    case file:read_file_info(DirName) of
        {ok, FI} when FI#file_info.type == directory ->
            true;
        _ ->
            false
    end.

forward_query(NewQuery, Args) ->
    OldQuery = [{Key, Val} || {Key, Val} <- yaws_api:parse_query(Args),
                              not lists:keymember(Key, 1, NewQuery)],
    to_query(NewQuery ++ OldQuery).

to_query([]) ->
    [];
to_query(Query) ->
    ["?", to_query2(Query)].

to_query2([{Key, Val} | Tail]) when is_atom(Key) ->
    to_query2([{atom_to_list(Key), Val} | Tail]);
to_query2([{Key, Val} | Tail]) when is_list(Key), is_list(Val) ->
    case Tail of
        [] ->
            [Key, "=", Val];
        _ ->
            [Key, "=", Val, "&", to_query2(Tail)]
    end.

to_html({kr, Int}) when is_integer(Int) ->
    [integer_to_list(Int div 100), ",",
     pad_right(abs(Int) rem 100, 2),
     "&nbsp;kr"];
to_html(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_html({Year, Month, Day}) ->
    [pad_right(Year, 4), "-", pad_right(Month, 2), "-", pad_right(Day, 2)];
%%to_html("") ->
%%    "&nbsp;";
to_html(String) when is_list(String) ->
    to_html_string(String);
to_html(Atom) when is_atom(Atom) ->
    case Atom of
        true    -> "1";
        false   -> "0";
        file    -> "Fil"; % BUGBUG
        account -> "Konto";  % BUGBUG
        voucher -> "Verifikation";  % BUGBUG
        item    -> "Kontering";  % BUGBUG
        budget  -> "Budget";  % BUGBUG
        _       -> atom_to_list(Atom)  % BUGBUG
    end.

to_html_string([195, Char | Tail]) ->
    case Char of
        165 -> ["&aring;" | to_html_string(Tail)]; % aa
        164 -> ["&auml;"  | to_html_string(Tail)]; % ae
        182 -> ["&ouml;"  | to_html_string(Tail)]; % oe

        133 -> ["&Aring;" | to_html_string(Tail)]; % AA
        132 -> ["&Auml;"  | to_html_string(Tail)]; % AE
        150 -> ["&Ouml;"  | to_html_string(Tail)]; % OE
        _   -> [195, Char | to_html_string(Tail)]
    end;
to_html_string([Char | Tail]) ->
    [Char | to_html_string(Tail)];
to_html_string([]) ->
    [].

pad_right(Int, N) ->
    string:right(integer_to_list(Int), N, $0).

%%-------------------------------------------------------------------
%% Import
%%-------------------------------------------------------------------

import_book(Args, Name) ->
    BooksDir = get_books_dir(Args),
    Delim = get_field_delim(Args),
    import_book(BooksDir, Name, Delim).

import_book(BooksDir, Name, Delim) ->
    Fun = fun(RelName) ->
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
    adapt_book(Name, Accounts, Vouchers, Items, Budgets, Types).

%%-------------------------------------------------------------------

to_tokens(Chars, Delim) ->
    Lines = string:tokens(Chars, "\n\r"),
    Tokens = [to_tokens(Line, Delim, [])
              || Line <- Lines],
    Tokens.

to_tokens([], _Delim, Tokens) ->
    lists:reverse(Tokens);
to_tokens([DoubleQuote = $" | Chars], Delim, Tokens) ->
    Pred = fun(Char) -> Char /= DoubleQuote end,
    case lists:splitwith(Pred, Chars) of
        {Token, [DoubleQuote | Tail]} ->
            trim_tokens(skip_spaces(Tail), Delim, [Token | Tokens]);
        {Token, []} ->
            trim_tokens([], Delim, [Token | Tokens])
        end;
to_tokens(Chars, Delim, Tokens) ->
    Pred = fun(Char) -> Char /= Delim end,
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
    error(account, Id, Arity, "bad arity, 7 expected", ?FILE, ?LINE);
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
    error(item, Id, Arity, "bad arity, 5 expected", ?FILE, ?LINE);
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
    error(voucher, Id, Arity, "bad arity, 3 expected", ?FILE, ?LINE);
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
    error(budget, Id, Arity, "bad arity, 2 expected", ?FILE, ?LINE);
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
    error(type, Id, Arity, "bad arity, 2 expected", ?FILE, ?LINE);
tokens_to_types([], Types) ->
    lists:reverse(Types).

to_int(Type, Id, Chars) ->
    case catch list_to_integer(Chars) of
        {'EXIT', _} ->
            error(Type, Id, Chars, "bad integer", ?FILE, ?LINE);
        Int ->
            Int
    end.

to_bool(Type, Id, Chars) ->
    case Chars of
        [$0] -> false;
        [$1] -> true;
         _   -> error(Type, Id, Chars, "bad boolean", ?FILE, ?LINE)
    end.

to_string(_Type, _Id, Chars) ->
    Chars. %string:strip(Chars, both, $").

to_ore(Type, Id, Chars) ->
    case string:tokens(Chars, ", ") of
        [Kr, Ore, "kr"] ->
            100 * to_int(Type, Id, Kr) + to_int(Type, Id, Ore);
        [Kr, "kr"] ->
            100 * to_int(Type, Id, Kr);
        [Kr] ->
            100 * to_int(Type, Id, Kr);
        _A->
            error(Type, Id, Chars, "bad value, should be like 123,45 kr",
                  ?FILE, ?LINE)
    end.

to_date(Type, Id, Chars) ->
    case string:tokens(Chars, "-: ") of
        [Year, Month, Day | _HourMinSec] ->
            {to_int(Type, Id, Year),
             to_int(Type, Id, Month),
             to_int(Type, Id, Day)};
        _ ->
            error(Type, Id, Chars,
                  "bad date, should be like YYYY-MM-DD HH:MM:SS", ?FILE, ?LINE)
    end.

error(Type, Id, Chars, Reason, File, Line) ->
    throw(#error{type = Type, id = Id, value = Chars, reason = Reason,
                 file = File, line = Line}).

%%-------------------------------------------------------------------
%% Adapt
%%-------------------------------------------------------------------

adapt_book(Name, Accounts, Vouchers, Items, Budgets, Types) ->
    {Accounts2, Errors}  = adapt_accounts(Accounts, []),
    {Vouchers2, Errors2} = adapt_vouchers(Vouchers, Errors),
    {Accounts3, Vouchers3, Errors3} =
        adapt_items(Accounts2, Vouchers2, Items, Errors2),
    {Accounts4, Errors4} = adapt_budgets(Accounts3, Budgets, Errors3),
    #book{name     = Name,
          accounts = lists:keysort(#account.id, Accounts4),
          vouchers = lists:keysort(#voucher.id, Vouchers3),
          types    = lists:keysort(#account_type.name, Types),
          errors   = lists:sort(Errors4)}.

adapt_accounts(Accounts, Errors) ->
    Errors2 = check_missing(Accounts, #account.id, #account.name,
                            account, "missing account name", Errors),
    Errors3 = check_missing(Accounts, #account.id, #account.type,
                            account, "missing account type", Errors2),
    {Accounts4, Errors4} =
        check_duplicates(Accounts, #account.id, #account.id,
                         account, "duplicate account id", error, Errors3),
    {Accounts5, Errors5} =
        check_duplicates(Accounts4, #account.id, #account.name,
                         account, "duplicate account name", warning, Errors4),
    {Accounts5,
     [#error{type   = account,
             id     = A#account.id,
             value  = A#account.balance,
             reason = "account should be included in result or balance",
             file   = ?FILE,
             line   = ?LINE}
      || A <- Accounts, A#account.balance == A#account.result] ++
     Errors5
    }.

check_missing(Tuples, IdPos, MandPos, Type, Reason, Errors) ->
    Errors2 = [#error{type   = Type,
                      id     = element(IdPos, T),
                      value  = element(MandPos, T),
                      reason = Reason,
                      file   = ?FILE,
                      line   = ?LINE}
              || T <- Tuples, element(MandPos, T) == ""],
    Errors2 ++ Errors.

check_duplicates(Tuples, IdPos, UniqPos, Type, Reason, Sev, Errors) ->
    Sorted = lists:keysort(UniqPos, Tuples),
    check_duplicates2(Sorted, IdPos, UniqPos, [], Type, Reason, Sev, Errors).

check_duplicates2([E | Tail], IdPos, UniqPos, Accounts, Type,
                  Reason, Sev, Errors)
  when is_record(E, error) ->
    check_duplicates2(Tail, IdPos, UniqPos, Accounts, Type,
                      Reason, Sev, [E | Errors]);
check_duplicates2([H, N | Tail], IdPos, UniqPos, Accounts, Type,
                  Reason, Sev, Errors)
  when element(UniqPos, H) == element(UniqPos, N) ->
    E = #error{type   = Type,
               id     = element(IdPos, H),
               value  = element(UniqPos, H),
               reason = Reason,
               file   = ?FILE,
               line   = ?LINE},
    Accounts2 = case Sev of
                    error   -> Accounts;
                    warning -> [H | Accounts]
                end,
    check_duplicates2([N | Tail], IdPos, UniqPos, Accounts2, Type,
                      Reason, Sev, [E | Errors]);
check_duplicates2([H | Tail], IdPos, UniqPos, Accounts, Type,
                  Reason, Sev, Errors) ->
    check_duplicates2(Tail, IdPos, UniqPos, [H | Accounts], Type,
                      Reason, Sev, Errors);
check_duplicates2([], _IdPos, _UniqPos, Accounts, _Type,
                  _Reason, _Sev, Errors) ->
    {Accounts, Errors}.

adapt_vouchers(Vouchers, Errors) ->
    Vouchers2 = lists:keysort(#voucher.id, Vouchers),
    Errors2 = check_missing(Vouchers2, #voucher.id, #voucher.text,
                           account, "missing voucher text", Errors),
    {Vouchers3, Errors3} = do_adapt_vouchers(Vouchers2, [], Errors2),
    {Vouchers3, Errors3}.

do_adapt_vouchers([E | Tail], Vouchers, Errors) when is_record(E, error) ->
    do_adapt_vouchers(Tail, Vouchers, [E |  Errors]);
do_adapt_vouchers([H, N | Tail], Vouchers, Errors)
  when N#voucher.id /= H#voucher.id + 1 ->
    E = #error{type   = voucher,
               id     = N#voucher.id,
               value  = N#voucher.id,
               reason = "not subsequent id",
               file   = ?FILE,
               line   = ?LINE},
    do_adapt_vouchers([N | Tail], [H | Vouchers], [E |  Errors]);
do_adapt_vouchers([H, N | Tail], Vouchers, Errors)
  when N#voucher.date < H#voucher.date ->
    E = #error{type   = voucher,
               id     = N#voucher.id,
               value  = N#voucher.date,
               reason = "date should be larger than date of previous voucher",
               file   = ?FILE,
               line   = ?LINE},
    do_adapt_vouchers([N | Tail], [H | Vouchers], [E |  Errors]);
do_adapt_vouchers([H = {_Y, M, _D} | Tail], Vouchers, Errors)
  when M < 1; M > 12 ->
    E = #error{type    = voucher,
               id     = H#voucher.id,
               value  = M,
               reason = "month not within range 1..12",
               file   = ?FILE,
               line   = ?LINE},
    do_adapt_vouchers(Tail, [H | Vouchers], [E |  Errors]);
do_adapt_vouchers([H = {_Y, _M, D} | Tail], Vouchers, Errors)
  when D < 1; D > 12 ->
    E = #error{type   = voucher,
               id     = H#voucher.id,
               value  = D,
               reason = "day not within range 1..12",
               file   = ?FILE,
               line   = ?LINE},
    do_adapt_vouchers(Tail, [H | Vouchers], [E |  Errors]);
do_adapt_vouchers([H | Tail], Vouchers, Errors) ->
    do_adapt_vouchers(Tail, [H | Vouchers], Errors);
do_adapt_vouchers([], Vouchers, Errors) ->
    {Vouchers, Errors}.

adapt_items(Accounts, Vouchers, Items, Errors) ->
    {Accounts2, Items2, Errors2} =
        add_missing_item_accounts(Items, Accounts, [], Errors),
    {Vouchers3, Items3, Error3} =
        add_missing_item_vouchers(Items2, Vouchers, [], Errors2),
    {Vouchers4, Errors4} =
        do_adapt_voucher_items(Accounts2, Vouchers3, Items3, [], Error3),
    {Accounts2, Vouchers4, Errors4}.

add_missing_item_accounts([I = #item{voucher_id = Vid,
                                     account_id = Aid} | Tail],
                          Accounts, Items, Errors) ->
    case lists:keymember(Aid, #account.id, Accounts) of
        true ->
            add_missing_item_accounts(Tail, Accounts, [I | Items], Errors);
        false ->
            E = #error{type = item,
                       id     = Vid,
                       value  = Aid,
                       reason = "reference to missing account",
                       file   = ?FILE,
                       line   = ?LINE},
            A = #account{id      = Aid,
                         name    = lists:concat(["ERROR_REF_BY_ITEM_",Aid]),
                         type    = "ERROR",
                         desc    = "",
                         old_id  = Aid,
                         result  = true,
                         balance = true},
            add_missing_item_accounts(Tail, [A | Accounts],
                                      [I | Items], [E | Errors])
    end;
add_missing_item_accounts([E | Tail], Accounts, Items, Errors)
  when is_record(E, error) ->
    add_missing_item_accounts(Tail, Accounts, Items, [E | Errors]);
add_missing_item_accounts([], Accounts, Items, Errors) ->
    {Accounts, lists:reverse(Items), Errors}.

add_missing_item_vouchers([I = #item{voucher_id = Vid} | Tail],
                          Vouchers, Items, Errors) ->
    case lists:keymember(Vid, #voucher.id, Vouchers) of
        true ->
            add_missing_item_vouchers(Tail, Vouchers, [I | Items], Errors);
        false ->
            E = #error{type  = item,
                       id    = Vid,
                       value = Vid,
                       reason = "reference to missing voucher",
                       file   = ?FILE,
                       line   = ?LINE},
            V = #voucher{id   = Vid,
                         date = erlang:date(),
                         text = "ERROR_REF_BY_ITEM"},
            add_missing_item_vouchers(Tail, [V | Vouchers],
                                      [I | Items], [E | Errors])
    end;
add_missing_item_vouchers([E | Tail], Accounts, Items, Errors)
  when is_record(E, error) ->
    add_missing_item_vouchers(Tail, Accounts, Items, [E | Errors]);
add_missing_item_vouchers([], Vouchers, Items, Errors) ->
    {Vouchers, lists:reverse(Items), Errors}.

do_adapt_voucher_items(Accounts, [V | Tail], Items, Vouchers, Errors)
  when V#voucher.items == undefined ->
    Vid = V#voucher.id,
    {VoucherItems, Items2, Errors2} = adapt_items(Items, Vid, [], [], Errors),
    V2 = V#voucher{items = VoucherItems},
    CalcSum = fun(I, Acc) -> Acc + I#item.amount end,
    case lists:foldl(CalcSum, 0, VoucherItems) of
        _  when VoucherItems == [] ->
            E = #error{type = voucher,
                       id = Vid,
                       value = Vid,
                       reason = "voucher should have items",
                       file   = ?FILE,
                       line   = ?LINE},
            do_adapt_voucher_items(Accounts, Tail, Items2, [V2 | Vouchers],
                                   [E | Errors2]);
        0 ->
            do_adapt_voucher_items(Accounts, Tail, Items2, [V2 | Vouchers],
                                   Errors2);
        NonZero ->
            E = #error{type = item,
                       id = Vid,
                       value = from_ore(NonZero),
                       reason = "sum of all items must be 0 within the voucher",
                       file   = ?FILE,
                       line   = ?LINE},
            do_adapt_voucher_items(Accounts, Tail, Items2, [V2 | Vouchers],
                                   [E | Errors2])
    end;
do_adapt_voucher_items(_Accounts, [], [], Vouchers, Errors) ->
    {Vouchers, Errors}.

adapt_items([I | Tail], Vid, Match, Rem, Errors)
  when I#item.voucher_id == Vid ->
    case I#item.amount of
        {0, 0} ->
            E = #error{type = item,
                       id = I#item.voucher_id,
                       value = 0,
                       reason = "bad debit and credit of account " ++
                           integer_to_list(I#item.account_id) ++
                           ", only one should be 0,00 kr",
                       file   = ?FILE,
                       line   = ?LINE},
            adapt_items(Tail, Vid, [I#item{amount = 0} | Match], Rem,
                        [E | Errors]);
        {Ore, 0} ->
            adapt_items(Tail, Vid, [I#item{amount = Ore} | Match], Rem,
                        Errors);
        {0, Ore} ->
            adapt_items(Tail, Vid, [I#item{amount = -Ore} | Match], Rem,
                        Errors);
        {Debit, Credit} ->
            E = #error{type = item,
                       id = I#item.voucher_id,
                       value = 0,
                       reason = "bad debit and credit of account " ++
                           integer_to_list(I#item.account_id) ++
                           ", one must be 0,00 kr",
                       file   = ?FILE,
                       line   = ?LINE},
            adapt_items(Tail, Vid, [I#item{amount = Debit - Credit} | Match],
                        Rem, [E | Errors])
        end;
adapt_items([I | Tail], Vid, Match, Rem, Errors) ->
    adapt_items(Tail, Vid, Match, [I | Rem], Errors);
adapt_items([], _Vid, Match, Rem, Errors) ->
    {lists:reverse(Match), lists:reverse(Rem), Errors}.

adapt_budgets(Accounts, Budgets, Errors) ->
    Pos = #budget.account_id,
    {Budgets2, Errors2} = check_duplicates(Budgets, Pos, Pos, budget,
                                          "duplicate budget id", error, Errors),
    {Accounts3, Errors3} = do_adapt_budgets(Budgets2, Accounts, Errors2),
    {Accounts3, Errors3}.

do_adapt_budgets([#budget{account_id = Aid, account_balance = Bal} | Tail],
                 Accounts, Errors) ->
    case [A || A <- Accounts, A#account.id == Aid] of
        [] ->
            E = #error{type = budget,
                       id = Aid,
                       value = Aid,
                       reason = "reference to missing account",
                       file   = ?FILE,
                       line   = ?LINE},
            A = #account{id      = Aid,
                         name    = lists:concat(["BUDGET_ERROR_",Aid]),
                         type    = "ERROR",
                         desc    = "",
                         old_id  = Aid,
                         result  = false,
                         balance = false,
                         budget = Bal},
            do_adapt_budgets(Tail, [A | Accounts], [E | Errors]);
        [A] ->
            Accounts2 = [X || X <- Accounts, X#account.id /= Aid],
            A2 = A#account{budget = Bal},
            case A2#account.result of
                true ->
                    do_adapt_budgets(Tail, [A2 | Accounts2], Errors);
                false ->
                    E = #error{type = budget,
                               id = Aid,
                               value = Aid,
                               reason = "reference to a account that"
                                        " not is included in result",
                               file   = ?FILE,
                               line   = ?LINE},
                    do_adapt_budgets(Tail, [A2 | Accounts2], [E | Errors])
            end
    end;
do_adapt_budgets([], Accounts, Errors) ->
    {Accounts, Errors}.

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
%%
%% book_to_chars(B, Delim) ->
%%     {[accounts_header(Delim), accounts_to_chars(B#book.accounts, Delim)],
%%      [vouchers_header(Delim), vouchers_to_chars(B#book.vouchers, Delim)],
%%      [items_header(Delim),    voucher_items_to_chars(B#book.vouchers, Delim)],
%%      [budgets_header(Delim),  budgets_to_chars(B#book.accounts, Delim)],
%%      [types_header(Delim),    types_to_chars(B#book.types, Delim)],
%%      [errors_header(Delim),   errors_to_chars(B#book.errors, Delim)]
%%     }.
%%
%% accounts_to_chars(Accounts, Delim) ->
%%     [
%%      [
%%       from_int(Id), Delim,
%%       from_string(Name), Delim,
%%       from_string(Type), Delim,
%%       from_string(Desc), Delim,
%%       from_int(OldId), Delim,
%%       from_bool(Result), Delim,
%%       from_bool(Balance), $\r, $\n
%%      ] || #account{id = Id,
%%                    name = Name,
%%                    type = Type,
%%                    desc = Desc,
%%                    old_id = OldId,
%%                    result = Result,
%%                    balance = Balance} <- Accounts].
%%
%% accounts_header(Delim) ->
%%     [
%%      from_string("Konto_Nr"), Delim,
%%      from_string("Konto_namn"), Delim,
%%      from_string("Konto_Typ"), Delim,
%%      from_string("K_beskrivning"), Delim,
%%      from_string("Gamla konto_Nr"), Delim,
%%      from_string("resultat"), Delim,
%%      from_string("balans"), $\r, $\n
%%     ].
%%
%% vouchers_to_chars(Vouchers, Delim) ->
%%     [
%%      [
%%       from_int(Id), Delim,
%%       from_date(Date), Delim,
%%       from_string(Text), $\r, $\n
%%      ] || #voucher{id   = Id,
%%                    date = Date,
%%                    text = Text} <- Vouchers].
%%
%% vouchers_header(Delim) ->
%%     [
%%      from_string("Verifikations_ID"), Delim,
%%      from_string("V_Datum"), Delim,
%%      from_string("V_Text"), $\r, $\n
%%     ].
%%
%% voucher_items_to_chars(Vouchers, Delim) ->
%%     [items_to_chars(V#voucher.items, Delim) || V <- Vouchers].
%%
%% items_to_chars(Items, Delim) ->
%%     [
%%      [
%%       from_int(Vid), Delim,
%%       from_int(Aid), Delim,
%%       from_ore(Ore, Delim), Delim,
%%       from_string(Remark), $\r, $\n
%%      ] || #item{voucher_id = Vid,
%%                 account_id = Aid,
%%                 amount     = Ore,
%%                 remark     = Remark} <- Items].
%%
%% items_header(Delim) ->
%%     [
%%      from_string("Verifikations_ID"), Delim,
%%      from_string("Konto_Nr"), Delim,
%%      from_string("Debet"), Delim,
%%      from_string("Kredit"), Delim,
%%      from_string("Kommentar"), $\r, $\n
%%     ].
%%
%% budgets_to_chars(Accounts, Delim) ->
%%     [
%%      [from_int(Id), Delim,
%%       from_int(Bal), $\r, $\n
%%      ] || #account{id     = Id,
%%                    budget = Bal} <- Accounts, Bal /= undefined
%%     ].
%%
%% budgets_header(Delim) ->
%%     [
%%      from_string("Konto_Nr"), Delim,
%%      from_string("Konto_saldo"), $\r, $\n
%%     ].
%%
%% types_to_chars(Types, Delim) ->
%%     [
%%      [from_string(Name), Delim,
%%       from_bool(Neg), $\r, $\n
%%      ] || #account_type{name    = Name,
%%                         negate  = Neg} <- Types
%%     ].
%%
%% types_header(Delim) ->
%%     [
%%      from_string("Konto_typ"), Delim,
%%      from_string("Konto_negativ"), $\r, $\n
%%     ].
%%
%% errors_to_chars(Errors, Delim) ->
%%     [
%%      [
%%       from_any(Type), Delim,
%%       from_any(Id), Delim,
%%       from_any(Val), Delim,
%%       from_string(Reason), $\r, $\n
%%      ] || #error{type   = Type,
%%                  id     = Id,
%%                  value  = Val,
%%                  reason = Reason} <- Errors].
%%
%% errors_header(Delim) ->
%%     [
%%      from_string("Typ"), Delim,
%%      from_string("Id"), Delim,
%%      from_string("Ajabaja"), Delim,
%%      from_string("Beskrivning"), $\r, $\n
%%     ].
%%
%% from_any(Int) when is_integer(Int) ->
%%     from_int(Int);
%% from_any(Date = {_, _, _}) ->
%%     from_date(Date);
%% from_any(String) when is_list(String) ->
%%     from_string(String);
%% from_any(Atom) when is_atom(Atom) ->
%%     case Atom of
%%         file    -> from_string("Fil");
%%         account -> from_string("Konto");
%%         voucher -> from_string("Verifikation");
%%         item    -> from_string("Kontering");
%%         budget  -> from_string("Budget");
%%         type    -> from_string("Kontotyp");
%%         _       -> from_string(atom_to_list(Atom))
%%     end.
%%
%% from_int(Int) ->
%%     integer_to_list(Int).
%%
%% from_string([]) ->
%%     [];
%% from_string(String) ->
%%     [$", String, $"].
%%
%% from_date({Year, Month, Day}) ->
%%     [
%%      from_int(Year),
%%      $-,
%%      from_int(Month),
%%      $-,
%%      from_int(Day)
%%      | " 00:00:00"
%%     ].
%%
%% from_bool(Bool) ->
%%     case Bool of
%%         false -> $0;
%%         true  -> $1
%%     end.
%%
%% from_ore(Ore, Delim) ->
%%     if
%%         Ore > 0 -> % Debit
%%             [from_ore(Ore), Delim, from_ore(0)];
%%         Ore < 0 -> % Credit
%%             [from_ore(0), Delim, from_ore(-Ore)];
%%         Ore == 0 ->
%%             [from_ore(0), Delim, from_ore(0)]
%%     end.
%%
from_ore(Ore) ->
    [integer_to_list(Ore div 100), $,, pad_right(Ore rem 100, 2), " kr"].
