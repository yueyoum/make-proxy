%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2016 上午11:52
%%%-------------------------------------------------------------------
-author("wang").

-record(http_request, {
    status = more       :: more | done | error,
    method              :: binary() | undefined,
    host                :: nonempty_string() | undefined,
    port                :: inet:port_number() | undefined,
    content_length = 0  :: non_neg_integer(),
    current_length = 0  :: non_neg_integer(),
    next_data = <<>>    :: binary()
}).
