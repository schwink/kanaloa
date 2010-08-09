NAME = kanaloa-dev
VERSION = 0.01

build:
	/bin/true

clean:
	/bin/true

install:
	/bin/true

package:
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
