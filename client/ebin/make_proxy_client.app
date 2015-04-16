{application,make_proxy_client,
             [{description,"Make Proxy Client"},
              {vsn,"2.0.0"},
              {modules,[make_proxy_client,mp_crypto,mpc_acceptor,
                        mpc_acceptor_sup,mpc_app,mpc_child,mpc_child_sup,
                        mpc_sup]},
              {registered,[mpc_sup,mpc_acceptor_sup,mpc_child_sup]},
              {applications,[kernel,stdlib]},
              {mod,{mpc_app,[]}}]}.
