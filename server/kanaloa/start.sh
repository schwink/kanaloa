#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa $PWD/tbin -boot start_sasl -s reloader -s start_kanaloa