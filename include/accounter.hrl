%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-define(XML_LOOKUP(Tag, List, Context),
        accounter_xml:lookup(Tag, List, Context, ?FILE, ?LINE)).

-define(BINDING(Tag, List),
        accounter:lookup_binding(Tag, List, ?FILE, ?LINE)).

-define(TO_HTML(IoList), accounter_html:to_html(IoList)).

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

-record(account_type,
        {name,
         negate}).

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

-record(error,
        {type,
         id,
         value,
         reason,
         file,
         line}).
