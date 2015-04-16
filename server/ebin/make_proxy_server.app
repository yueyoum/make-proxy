{application,make_proxy_server,
             [{description,"Make Proxy Server"},
              {vsn,"2.0.0"},
              {modules,[make_proxy_server,mp_app,mp_child,mp_crypto,mp_sup]},
              {registered,[mp_sup]},
              {applications,[kernel,stdlib]},
              {mod,{mp_app,[]}}]}.
