NAME = kanaloa-dev-service
VERSION = 0.01
DEST = $(DESTDIR)/usr/lib/erlang/lib/$(NAME)-$(VERSION)
DEST_EBIN = $(DEST)/ebin
DEST_BIN = $(DESTDIR)/usr/bin
DEST_NGINX=$(DESTDIR)/etc/nginx/sites-available
DEST_SHARE = $(DESTDIR)/usr/share/$(NAME)/$(VERSION)

build: ebin/
	(cd src;$(MAKE) all)

clean:
	(cd src;$(MAKE) clean)

ebin/:
	@mkdir -p ebin

install:
	@mkdir -p $(DEST_EBIN) || true
	cp ebin/* $(DEST_EBIN) || true
	@mkdir -p $(DEST_BIN) || true
	cp bin/kanaloa-test-app $(DEST_BIN)/
	@mkdir -p $(DEST_NGINX) || true
	cp config/nginx/kanaloa-dev $(DEST_NGINX)/
	mkdir -p $(DEST_SHARE)/test || true
	@cp -r test/* $(DEST_SHARE)/test/

deb:
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
