SHELL = /bin/bash

all: server client

.PHONY: server
server:
	cd server && ./rebar compile

.PHONY: client
client:
	cd client && ./rebar compile

.PHONY: clean
clean:
	cd server && ./rebar clean
	cd client && ./rebar clean

