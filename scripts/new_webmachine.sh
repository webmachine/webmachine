#!/usr/bin/env bash

WS=mochiweb
# Set WSAPP to the whole rebar variable to allow it to be
# set completely empty for particular web servers. This is
# because rebar doesn't seem to like command-line variables
# of the form "foo=" to try to set an empty value. Also
# note the trailing comma on the WSAPP setting is needed if
# it's not empty.
WSAPP='webserver_app=mochiweb,'

usage() {
    cat <<EOF
usage: $0 [-s webserver] name [destdir]
  supported web servers: mochiweb, yaws
EOF
}

while getopts ":hs:" option; do
    case $option in
        h)
            usage
            exit 1 ;;
        s)
            case $OPTARG in
                mochiweb)
                    : ;;
                yaws)
                    WS=yaws
                    WSAPP="" ;;
                *)
                    echo error: $0: $OPTARG is not a supported web server
                    usage
                    exit 1 ;;
            esac ;;
        ?)
            usage
            exit 0 ;;
    esac
done
shift $(($OPTIND - 1))

SCRIPT=${0##*/}
NAME=$1
DESTDIR=${2:-.}

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

[[ $DESTDIR =~ /${NAME}$ ]] && DESTDIR=${DESTDIR%/*}

[[ -e $DESTDIR ]] || mkdir -p $DESTDIR

ABSDEST=$(cd $DESTDIR && pwd)

PREFIX=$ABSDEST/$NAME

cd ${0%/*}/../priv

exec ../rebar create template=wmskel appid=$NAME prefix=$PREFIX webserver=$WS $WSAPP
