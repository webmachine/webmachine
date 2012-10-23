#!/usr/bin/env bash

SCRIPT=${0##*/}
NAME=$1
DESTDIR=$2

usage() {
    echo "usage: new_webmachine.sh name [destdir]"
}

if [ -z $NAME ] || [[ $NAME =~ ^[\.\~\/] ]]; then
    usage
    exit 1
fi

erl -noshell -eval 'halt(if is_atom('"$NAME"') -> 0; true -> 1 end).'
if [[ $? -ne 0 ]]; then
    echo $SCRIPT: \""$NAME"\" is not allowed as a project name
    echo '  The project name must begin with a lowercase letter and'
    echo '  contain only alphanumeric characters and underscores.'
    usage
    exit 1
fi

if [ -z $DESTDIR ]; then
    DESTDIR="."
elif [[ $DESTDIR =~ /${NAME}$ ]]; then
    DESTDIR=${DESTDIR%/*}
fi

if [ ! -e $DESTDIR ]; then
    $(mkdir -p $DESTDIR)
fi

ABSDEST=$(cd $DESTDIR && pwd)

cd ${0%/*}/../priv

../rebar create template=wmskel appid=$NAME prefix=$ABSDEST/$NAME
