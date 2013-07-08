#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"


erl -pa common -pa client/ebin -config client +K true -eval "application:start(make_proxy_client)." -noshell

exit 0

