#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"


running=`ps -ef | grep make_proxy_client | wc -l`
if [[ running -ge 2 ]]
then
    echo "alreay running"
    exit -1
else
    erl -pa client/ebin -config client +K true -s make_proxy_client start
fi

exit 0

