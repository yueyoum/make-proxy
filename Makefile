SHELL = /bin/bash

all: server client

.PHONY: server
server:
	erlc -o server/ebin server/src/*.erl
	erlc -o common common/*.erl

.PHONY: client
client:
	erlc -o client/ebin client/src/*.erl
	erlc -o common common/*.erl
