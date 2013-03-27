
-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-define(OPTIONS,
		[inet,
		binary,
		{reuseaddr, true},
		{active, false},
		{nodelay, true},
		{recbuf, 32768},
		{sndbuf, 32768},
		{packet_size, 32768}]
		).

-define(OPTIONS(IP), [{ip, IP} | ?OPTIONS]).

-define(GETADDR, fun(IP) -> {ok, Addr} = inet:getaddr(IP, inet), Addr end).
