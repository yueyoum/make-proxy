# Make Proxy

## Features

with the great erlang, the project has the following features:

*   support http, https, socks4, socks5,http proxy.
*   Robustness. never down.
*   Scalable. handle thousands requests at the same time easily.
*   Fast.
*   Lightweight.


## Illustrate

```
+------------+            +--------------+          
| local app  |  <=======> | proxy client | <#######
+------------+            +--------------+        #
                                                  #
                                                  #
                                                  # encrypted data
                                                  #
                                                  #
+-------------+            +--------------+       #
| target host |  <=======> | proxy server |  <#####
+-------------+            +--------------+         
```


## Usage

1.  git clone https://github.com/yueyoum/make-proxy.git
2.  cd make-proxy
3.  rebar3 tree && rebar3 compile
4.  cp app.config.example app.config
    
    #### app.config

    *   server_addr - which address that the server listen on
    *   server_port - which port that the server listen on
    *   client_port - which port that the client listen on
    *   key - key to encrypt/decrypt data **16bytes**

5.  run `./start_server.sh` at server side, and `./start_client.sh` at client side.
6.  Done.

## TODO

1.  Support Socks5 Username/Password Authorize
2.  Client & Server using ssl connection
3.  Traffic Statistics
