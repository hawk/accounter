%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-record(book,
        {name,
         accounts,
         vouchers,
         types,
         errors}).

-record(account,
        {id,
         name,
         type,
         desc,
         old_id,
         result,
         balance,
         budget}).

-record(item,
        {voucher_id,
         account_id,
         amount, % Negative if credit
         remark}).

-record(voucher,
        {id,
         date,
         text,
         items}).

-record(budget,
        {account_id,
         account_balance}).

-record(account_type,
        {name,
         negate}).

-record(error,
        {type,
         id,
         value,
         reason}).
