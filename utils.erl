% -module(utils).

-export([options/0,
		 options/1,
		 get_config/0,
		 cover_to_ip_address/1]).

-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).


options() ->
	[
		inet,
		binary,
		{reuseaddr, true},
		{active, false},
		{nodelay, true}
	].

options(BindIP) ->
	[{ip, BindIP} | options()].


get_config() ->
	% FIXME
	{{127,0,0,1}, 7070, {127,0,0,1}, 7071}.



cover_to_ip_address(IPStr) ->
    {ok, IP} = inet:getaddr(IPStr, inet),
    IP.
