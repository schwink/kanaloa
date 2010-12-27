#!/bin/bash

set -e

cwd=`pwd`

rm -f $cwd/*.deb
rm -f $cwd/*.changes
rm -f $cwd/server/*.deb
rm -f $cwd/server/*.changes
rm -f $cwd/client/*.deb
rm -f $cwd/client/*.changes

cd $cwd/server/kanaloa && make deb
cd $cwd/server/dev && make deb
cd $cwd/server/dev-service && make deb
cd $cwd/client/web && make deb

cd $cwd

mv $cwd/server/*.deb $cwd
mv $cwd/server/*.changes $cwd
mv $cwd/client/*.deb $cwd
mv $cwd/client/*.changes $cwd
