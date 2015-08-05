%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-define(XML_LOOKUP(Tag, List, Context),
        accounter_xml:lookup(Tag, List, Context, ?FILE, ?LINE)).

-define(BINDING(Tag, List),
        accounter:lookup_binding(Tag, List, ?FILE, ?LINE)).

-define(TO_HTML(IoList), accounter_html:to_html(IoList)).

-define(FQ(Args, Query), accounter_html:forward_query(Args, Query)).

-record(account_type,
        {name       :: string(),
         negate     :: boolean(),
         in_balance :: boolean()}).

-record(account,
        {id         :: non_neg_integer(),
         old_id     :: non_neg_integer(),
         name       :: string(),
         desc       :: string(),
         type       :: string(),
         budget     :: integer(),
         budget_remark :: string(),
         in_result  :: boolean(),
         in_balance :: boolean()}).

-record(item,
        {voucher_id :: non_neg_integer(),
         account_id :: non_neg_integer(),
         amount     :: integer(), % Negative if credit
         remark     :: string()}).

-record(voucher,
        {id    :: non_neg_integer(),
         date  :: {Year  ::non_neg_integer(),
                   Month ::non_neg_integer(),
                   Day   ::non_neg_integer()},
         text  :: string(),
         items :: [#item{}]}).

-record(budget,
        {account_id      :: non_neg_integer(),
         account_balance :: integer()}).

-record(error,
        {type   :: string(),
         id     :: non_neg_integer(),
         value  :: any(),
         reason :: string(),
         file   :: file:filename(),
         line   :: non_neg_integer()}).

-record(book,
        {name     :: string(),
         types    :: [#account_type{}],
         accounts :: [#account{}],
         vouchers :: [#voucher{}],
         errors   :: [#error{}]}).
