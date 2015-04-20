#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"

erl -pa server/ebin -config server +K true -s make_proxy_server start -detached
exit $?
