NAME = kanaloa-dev
VERSION = 0.01
NGINX_SITES_AVAILABLE=$(DESTDIR)/etc/nginx/sites-available
DEST_BIN = $(DESTDIR)/usr/bin

build:
	/bin/true

clean:
	/bin/true

force:

install:
	@mkdir -p $(NGINX_SITES_AVAILABLE) || true
	cp config/nginx/kanaloa $(NGINX_SITES_AVAILABLE)/ || true
	@mkdir -p $(DEST_BIN) || true
	cp bin/kanaloa_test_app $(DEST_BIN)

package: force
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
