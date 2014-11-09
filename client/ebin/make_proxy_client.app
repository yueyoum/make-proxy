{
    application,
    make_proxy_client,
    [
        {description, "Make Proxy Client"},
        {vsn, "2.0.0"},
        {modules, [
            mpc_app,
            mpc_sup,
            mpc_child
        ]},
        {registered, [mpc_sup]},
        {applications, [kernel, stdlib]},
        {mod, {mpc_app, []}}
    ]
}.
