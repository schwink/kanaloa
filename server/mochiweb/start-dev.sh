#!/bin/sh
cd `dirname $0`
make
exec erl -pa $PWD/ebin -boot start_sasl -s reloader -s kanaloa
