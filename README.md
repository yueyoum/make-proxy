# Socks5 proxy written in Erlang

## Intro

This project can take you through a Firewall using Socks5 proxy.

### Features

with the great erlang, the project has the following features:

*   robustness. never down.
*   scalable. handle thousands requests at the same time easily.
*   fast.

### Running status

In my daily use, 

*   Play a youtube **480P** vedio, and load some other websites at the same time,
    It only takes 20MB ~ 30MB memories.

*   Play a youtube **720P** vedio, bellow is the system status.

    Linode 4 cpus, 512 RAM VPS.

    **Update**

    this maybe different for different length 720P vedios.
    Short will only takes about 20MB ram, But a long one, will takes almost 140MB.
    [why takes so much memories](#why-takes-so-much-memories)

    ![system status](http://i1297.photobucket.com/albums/ag23/yueyoum/uuu_zpsd70d73bb.png)




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



## Why takes so much memories

as mentioned above, Play youtube **720P** vedios will take at most 140MB memories.
Why this?

The server side has a `process pool`, 
When a process finish it's job, It's not died directly, But waiting a WORKER_TIMEOUT times,
If in the WORKER_TIMEOUT, there is a new request comes to this process, 
the process will work again, and when finish, waiting for new request in WORKER_TIMEOUT.

If passed the WORKER_TIMEOUT, no request comes in, then this process will die,
and erlang GC can working with this process.

This is why, when doing a heavy transfer (e.g, 720P vedios),
the process will worker in many circles,
in this time, GC can not work with this process, memory usage will growing.


