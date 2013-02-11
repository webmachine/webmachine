#!/bin/sh
cd `dirname $0`
WEBMACHINE_SERVER={{webserver}}
export WEBMACHINE_SERVER
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s {{appid}}
