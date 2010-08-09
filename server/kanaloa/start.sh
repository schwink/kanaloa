#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa $PWD/tbin \
    -boot start_sasl -s reloader -s test_app \
    -test_app port 8001
