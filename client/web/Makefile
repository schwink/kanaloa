NAME = kanaloa-client-web
VERSION = 0.01
DEST_SHARE = $(DESTDIR)/usr/share/javascript/kanaloa/$(VERSION)/

build:
	/bin/true

clean:
	/bin/true

force:

install:
	mkdir -p $(DEST_SHARE) || true
	cp -r src/* $(DEST_SHARE)

package: force
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
