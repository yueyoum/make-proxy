{
    application,
    make_proxy_client,
    [
        {description, "Make Proxy Client"},
        {vsn, "2.0.0"},
        {modules, [
            mpc_app,
            mpc_sup,
            mpc_acceptor_sup,
            mpc_child_sup,
            mpc_acceptor,
            mpc_child
        ]},
        {registered, [mpc_sup, mpc_acceptor_sup, mpc_child_sup]},
        {applications, [kernel, stdlib]},
        {mod, {mpc_app, []}}
    ]
}.
