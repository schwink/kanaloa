NAME = kanaloa-client-web
VERSION = 0.01
DEST_SHARE = $(DESTDIR)/usr/share/$(NAME)/$(VERSION)
DEST_SHARE_JS = $(DESTDIR)/usr/share/javascript/kanaloa/$(VERSION)
DEST_SHARE_DOC = $(DESTDIR)/usr/share/doc/$(NAME)/$(VERSION)/html

build:
	/bin/true

clean:
	rm -rf doc

force:

doc: force
	@mkdir doc 2>/dev/null || true
	jsdoc -d=doc -a src/kanaloa.js

install:
	mkdir -p $(DEST_SHARE_JS) || true
	cp -r src/* $(DEST_SHARE_JS)/
	mkdir -p $(DEST_SHARE)/test/ || true
	cp -r test/* $(DEST_SHARE)/test/
	mkdir -p $(DEST_SHARE_DOC) || true
	cp -r doc/* $(DEST_SHARE_DOC)/

package: force
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
