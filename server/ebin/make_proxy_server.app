{
    application,
    make_proxy_server,
    [
        {description, "Make Proxy Server"},
        {vsn, "0.1.0"},
        {modules, [
            make_proxy_server,
            mp_app,
            mp_sup,
            mp_accept,
            mp_child,
            mp_child_sup
        ]},
        {registered, [mp_sup]},
        {applications, [kernel, stdlib]},
        {mod, {mp_app, []}}
    ]
}.
