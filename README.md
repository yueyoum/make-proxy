# Socks5 proxy written in Erlang

## Intro

This project can take you through a Firewall via Socks5 proxy.

### Features

with the great erlang, the project has the following features:

*   Robustness. never down.

*   Scalable. handle thousands requests at the same time easily.

*   Fast.

*   Lightweight. only needs at most 30MB memories.


### Running status

In my daily use, 

*   When server is idle, It's only takes about 10MB memories.

*   Play a youtube **1080P** video, and load some other websites at the same time,
    It only takes 20MB ~ 30MB memories.

    Below is the system status when play a youtube **1080P** video.
    Notice the `beam.smp` process below of `mysqld`.

    Linode 4 cpus, 512 RAM VPS.

    ![system status](http://i1297.photobucket.com/albums/ag23/yueyoum/uuu_zps1908ecbd.png)




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

    It receive your app (like browser) request, encrypt the data,
    send to `proxy server`

2.  `proxy server` receive the request from `proxy client`,
    decrypt it, and sent to the target host.

3.  `proxy server` got the response from target host, and encrypt response,
    send back to `proxy client`.

4.  `proxy client` decrypt response received from `proxy server`,
    and send to local app.

5.  the circle done.


## Usage

### Server side

1.  `git clone https://github.com/yueyoum/make-proxy.git` or directly download.
2.  `cp src/config.hrl.example src/config.hrl`

    You need to define the `REMOTEIP` and `REMOTEPORT`.

    `REMOTEPORT` is which port proxy_server will listen on.

4.  `./start_server.sh` or `./start_server.sh -d` will run the server in backend.


### Local side

1.  same as the *Serve side*, checkout the code, and do the difinition **AS SAME AS** the server side

2.  `./start_client.sh`


Now, you can set your apps (e.g. Browser) Using socks5 proxy.

IP = `127.0.0.1`

PORT = `7070`  (if not changed in the src/config.hrl)


## TODO

1.  change the decrypt method, now is just  every byte do `bxor` with `2#01111001`


