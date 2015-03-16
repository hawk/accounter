%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter_check).
-export([
         amend_book/6,
         amend_items/5
        ]).

-include("../include/accounter.hrl").

%%-------------------------------------------------------------------
%% Amend
%%-------------------------------------------------------------------

amend_book(Name, Types, Accounts, Budgets, Vouchers, Items) ->
    {Accounts2, Errors}  = amend_accounts(Accounts, []),
    {Vouchers2, Errors2} = amend_vouchers(Vouchers, Errors),
    {Accounts3, Vouchers3, Errors3} =
        amend_items(Accounts2, Vouchers2, Items, Errors2),
    {Accounts4, Errors4} = amend_budgets(Accounts3, Budgets, Errors3),
    #book{name     = Name,
          types    = lists:keysort(#account_type.name, Types),
          accounts = lists:keysort(#account.id, Accounts4),
          vouchers = lists:keysort(#voucher.id, Vouchers3),
          errors   = lists:sort(Errors4)}.

amend_accounts(Accounts, Errors) ->
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
             value  = A#account.in_balance,
             reason = "account should be included in result or balance",
             file   = ?FILE,
             line   = ?LINE}
      || A <- Accounts,
         A#account.in_balance =:= A#account.in_result] ++
         Errors5
    }.

check_missing(Tuples, IdPos, MandPos, Type, Reason, Errors) ->
    Errors2 = [#error{type   = Type,
                      id     = element(IdPos, T),
                      value  = element(MandPos, T),
                      reason = Reason,
                      file   = ?FILE,
                      line   = ?LINE}
              || T <- Tuples, element(MandPos, T) =:=  ""],
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
  when element(UniqPos, H) =:=  element(UniqPos, N) ->
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

amend_vouchers(Vouchers, Errors) ->
    Vouchers2 = lists:keysort(#voucher.id, Vouchers),
    Errors2 = check_missing(Vouchers2, #voucher.id, #voucher.text,
                            voucher, "missing voucher text", Errors),
    {Vouchers3, Errors3} = do_amend_vouchers(Vouchers2, [], Errors2),
    {Vouchers3, Errors3}.

do_amend_vouchers([E | Tail], Vouchers, Errors) when is_record(E, error) ->
    do_amend_vouchers(Tail, Vouchers, [E |  Errors]);
do_amend_vouchers([H, N | Tail], Vouchers, Errors)
  when N#voucher.id =/= H#voucher.id + 1 ->
    E = #error{type   = voucher,
               id     = N#voucher.id,
               value  = N#voucher.id,
               reason = "not subsequent id",
               file   = ?FILE,
               line   = ?LINE},
    do_amend_vouchers([N | Tail], [H | Vouchers], [E |  Errors]);
do_amend_vouchers([H, N | Tail], Vouchers, Errors)
  when N#voucher.date < H#voucher.date ->
    E = #error{type   = voucher,
               id     = N#voucher.id,
               value  = N#voucher.date,
               reason = "date should be larger than date of previous voucher",
               file   = ?FILE,
               line   = ?LINE},
    do_amend_vouchers([N | Tail], [H | Vouchers], [E |  Errors]);
do_amend_vouchers([H = {_Y, M, _D} | Tail], Vouchers, Errors)
  when M < 1; M > 12 ->
    E = #error{type    = voucher,
               id     = H#voucher.id,
               value  = M,
               reason = "month not within range 1..12",
               file   = ?FILE,
               line   = ?LINE},
    do_amend_vouchers(Tail, [H | Vouchers], [E |  Errors]);
do_amend_vouchers([H = {_Y, _M, D} | Tail], Vouchers, Errors)
  when D < 1; D > 12 ->
    E = #error{type   = voucher,
               id     = H#voucher.id,
               value  = D,
               reason = "day not within range 1..12",
               file   = ?FILE,
               line   = ?LINE},
    do_amend_vouchers(Tail, [H | Vouchers], [E |  Errors]);
do_amend_vouchers([H | Tail], Vouchers, Errors) ->
    do_amend_vouchers(Tail, [H | Vouchers], Errors);
do_amend_vouchers([], Vouchers, Errors) ->
    {Vouchers, Errors}.

amend_items(Accounts, Vouchers, Items, Errors) ->
    {Accounts2, Items2, Errors2} =
        add_missing_item_accounts(Items, Accounts, [], Errors),
    {Vouchers3, Items3, Error3} =
        add_missing_item_vouchers(Items2, Vouchers, [], Errors2),
    {Vouchers4, Errors4} =
        do_amend_voucher_items(Accounts2, Vouchers3, Items3, [], Error3),
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
                         in_result  = true,
                         in_balance = true},
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

do_amend_voucher_items(Accounts, [V | Tail], Items, Vouchers, Errors)
  when V#voucher.items =:=  undefined ->
    Vid = V#voucher.id,
    {VoucherItems, Items2, Errors2} = amend_items(Items, Vid, [], [], Errors),
    V2 = V#voucher{items = VoucherItems},
    CalcSum = fun(I, Acc) -> Acc + I#item.amount end,
    case lists:foldl(CalcSum, 0, VoucherItems) of
        _  when VoucherItems =:=  [] ->
            E = #error{type = voucher,
                       id = Vid,
                       value = Vid,
                       reason = "voucher should have items",
                       file   = ?FILE,
                       line   = ?LINE},
            do_amend_voucher_items(Accounts, Tail, Items2, [V2 | Vouchers],
                                   [E | Errors2]);
        0 ->
            do_amend_voucher_items(Accounts, Tail, Items2, [V2 | Vouchers],
                                   Errors2);
        NonZero ->
            E = #error{type = voucher,
                       id = Vid,
                       value = accounter:from_ore(NonZero),
                       reason = "sum of all items must be 0 within the voucher",
                       file   = ?FILE,
                       line   = ?LINE},
            do_amend_voucher_items(Accounts, Tail, Items2, [V2 | Vouchers],
                                   [E | Errors2])
    end;
do_amend_voucher_items(_Accounts, [], [], Vouchers, Errors) ->
    {Vouchers, Errors}.

amend_items([I | Tail], Vid, Match, Rem, Errors)
  when I#item.voucher_id =:=  Vid ->
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
            amend_items(Tail, Vid, [I#item{amount = 0} | Match], Rem,
                        [E | Errors]);
        {Ore, 0} ->
            amend_items(Tail, Vid, [I#item{amount = Ore} | Match], Rem,
                        Errors);
        {0, Ore} ->
            amend_items(Tail, Vid, [I#item{amount = -Ore} | Match], Rem,
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
            amend_items(Tail, Vid, [I#item{amount = Debit - Credit} | Match],
                        Rem, [E | Errors])
        end;
amend_items([I | Tail], Vid, Match, Rem, Errors) ->
    amend_items(Tail, Vid, Match, [I | Rem], Errors);
amend_items([], _Vid, Match, Rem, Errors) ->
    {lists:reverse(Match), lists:reverse(Rem), Errors}.

amend_budgets(Accounts, Budgets, Errors) ->
    Pos = #budget.account_id,
    {Budgets2, Errors2} = check_duplicates(Budgets, Pos, Pos, budget,
                                          "duplicate budget id", error, Errors),
    {Accounts3, Errors3} = do_amend_budgets(Budgets2, Accounts, Errors2),
    {Accounts3, Errors3}.

do_amend_budgets([#budget{account_id = Aid, account_balance = Bal} | Tail],
                 Accounts, Errors) ->
    case [A || A <- Accounts, A#account.id =:=  Aid] of
        [] ->
            E = #error{type = budget,
                       id = Aid,
                       value = Aid,
                       reason = "reference to missing account",
                       file   = ?FILE,
                       line   = ?LINE},
            A = #account{id                 = Aid,
                         name               = lists:concat(["BUDGET_ERROR_",Aid]),
                         type       = "ERROR",
                         desc       = "",
                         old_id     = Aid,
                         in_result  = false,
                         in_balance = false,
                         budget             = Bal},
            do_amend_budgets(Tail, [A | Accounts], [E | Errors]);
        [A] ->
            Accounts2 = [X || X <- Accounts, X#account.id =/= Aid],
            A2 = A#account{budget = Bal},
            case A2#account.in_result of
                true ->
                    do_amend_budgets(Tail, [A2 | Accounts2], Errors);
                false ->
                    E = #error{type = budget,
                               id = Aid,
                               value = Aid,
                               reason = "reference to a account that"
                                        " not is included in result",
                               file   = ?FILE,
                               line   = ?LINE},
                    do_amend_budgets(Tail, [A2 | Accounts2], [E | Errors])
            end
    end;
do_amend_budgets([], Accounts, Errors) ->
    {Accounts, Errors}.
