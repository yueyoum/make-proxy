{
    application,
    make_proxy_client,
    [
        {description, "Make Proxy Client"},
        {vsn, "0.1.0"},
        {modules, [
            make_proxy_cient,
            mpc_app,
            mpc_sup,
            mpc_accept,
            mpc_child,
            mpc_child_sup
        ]},
        {registered, [mpc_sup]},
        {applications, [kernel, stdlib]},
        {mod, {mpc_app, []}}
    ]
}.