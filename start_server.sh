#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"


usage()
{
    echo "start_server"
    echo "    -h    show this help"
    echo "    -d    run server as daemon"
}


DETACH=0
while getopts "hd" arg
do
    case $arg in
        h)
            usage
            exit 0
            ;;
        d)
            DETACH=1
            ;;
        ?)
            usage
            exit 1
            ;;
    esac
done


cd src
erlc transform.erl

if [[ $DETACH == 1 ]]
then
    erlc server.erl
else
    erlc -Ddebug server.erl
fi


exit_code=$?
[[ $exit_code != 0 ]] && exit $exit_code


if [[ $DETACH == 1 ]]
then
    erl -noshell +K true -s server start -detached
else
    erl -noshell +K true -s server start
fi

exit 0

