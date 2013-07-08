# Socks5 proxy written in Erlang

## Intro

This project can take you through a Firewall via Socks5 proxy.

### Features

with the great erlang, the project has the following features:

*   Robustness. never down.

*   Scalable. handle thousands requests at the same time easily.

*   Fast.

*   Lightweight. only takes less than 20MB memories.


### Running status

In my daily use, 

*   When server is idle, It only takes about 10MB memories.

*   Play a youtube **1080P** video, 
    It only takes 12MB memories.

    Below is the system status when play a youtube **1080P** video.
    Notice the `beam.smp` process.

    Linode 8 cpus, 1G RAM VPS.

    ![system status](http://i1297.photobucket.com/albums/ag23/yueyoum/status_zpsa77d8243.png)




### Illustrate

```
+-----------+            +--------------+   encrypt
| local app |  <=======> | proxy client |  <#######
+-----------+   decrypt  +--------------+         #
                                                  #
                                                  #
                                                  # encrypted data
                                                  #
                                                  #
+-------------+            +--------------+       #
| target host |  <=======> | proxy server |  <#####
+-------------+   decrypt  +--------------+  encrypt
```

1.  `proxy client` is running at your local computer.

    It receive your app (like a browser) request, encrypt the data,
    send to `proxy server`

2.  `proxy server` receive the request from `proxy client`,
    decrypt it, and sent to the target host.

3.  `proxy server` got the response from target host, and encrypt response,
    send back to `proxy client`.

4.  `proxy client` decrypt response received from `proxy server`,
    and send to local app.

5.  the circle done.

## OTP Application Schema

![schema](http://i1297.photobucket.com/albums/ag23/yueyoum/x_zps84037781.png)

`mp_sup` is the application's **top** supervisor,

`mp_accept` block on `gen_tcp:accept`, when new connection comes,

`mp_child_sup` will start a new child, this child communicate with 
the connected client, and die after connection closed, or error ocurred.

## Usage

### Server side

1.  `git clone https://github.com/yueyoum/make-proxy.git` or directly download.

2.  `cd make-proxy`

3.  `cp server.config.example server.config`,
    and change the port what you want.

    server will listen on this port.

4.  `make server`
5.  `./start_server.sh`
6.  Done.

### Local side

1.  same as the *Server side*, checkout the code.

2.  `cd make-proxy`

3.  `cp client.config.example client.config`
    * remote_addr: IP of the compute where `make-proxy` server runs
    * remote_port: PORT that make-proxy server using. **SAME** as the defination of `server.config`.
    * local_addr: "127.0.0.1" or "0.0.0.0" or some specified address.
    * port: which port the make-proxy client will listen on.

4.  `make client`

5.  `./start_client.sh`

6.  Done

Now, you can set your apps (e.g. Browser) Using socks5 proxy.

IP = `127.0.0.1`
PORT = `7070`  (if not changed in the client.config)


## TODO

1.  Support HTTP Proxy
