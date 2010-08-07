NAME = kanaloa-client-web
VERSION = 0.01
DEST_SHARE_JS = $(DESTDIR)/usr/share/javascript/kanaloa/$(VERSION)
DEST_SHARE_KANALOA = $(DESTDIR)/usr/share/kanaloa/$(VERSION)

build:
	/bin/true

clean:
	/bin/true

force:

install:
	mkdir -p $(DEST_SHARE_JS) || true
	cp -r src/* $(DEST_SHARE_JS)/
	mkdir -p $(DEST_SHARE_KANALOA)/test/ || true
	cp -r test/* $(DEST_SHARE_KANALOA)/test/

package: force
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
