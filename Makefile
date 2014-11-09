SHELL = /bin/bash

all: server client

.PHONY: server
server:
	erlc -o server/ebin +debug_info server/src/*.erl
	erlc -o server/ebin +debug_info common/*.erl

.PHONY: client
client:
	erlc -o client/ebin +debug_info client/src/*.erl
	erlc -o client/ebin common/*.erl

.PHONY: clean
clean:
	-rm -f client/ebin/*.beam
	-rm -f server/ebin/*.beam

