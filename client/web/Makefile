NAME = kanaloa-client-web
VERSION = 0.01
DEST_SHARE = $(DESTDIR)/usr/share/$(NAME)/$(VERSION)
DEST_SHARE_JS = $(DESTDIR)/usr/share/javascript/kanaloa/$(VERSION)
DEST_SHARE_DOC = $(DESTDIR)/usr/share/doc/$(NAME)/$(VERSION)/html

build: out/
	cp src/kanaloa.js out/
	yuicompressor -o out/kanaloa.min.js out/kanaloa.js

force:

doc: force
	jsdoc -d=doc -a src/kanaloa.js

clean:
	rm -rf out
	rm -rf doc

out/:
	@mkdir -p out

install:
	mkdir -p $(DEST_SHARE_JS) || true
	@cp -r out/* $(DEST_SHARE_JS)/
	mkdir -p $(DEST_SHARE)/test/ || true
	@cp -r test/* $(DEST_SHARE)/test/
	mkdir -p $(DEST_SHARE_DOC) || true
	@cp -r doc/* $(DEST_SHARE_DOC)/

package:
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
