#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"

cd src
erlc transform.erl
erlc client.erl

exit_code=$?
[[ $exit_code != 0 ]] && exit $exit_code

erl -noshell +K true -s client start

exit 0

