#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"

erl -pa common -pa server/ebin -noshell -config server +K true -eval "application:start(make_proxy_server)." -detached
exit $?
