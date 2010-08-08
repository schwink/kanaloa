NAME = kanaloa-dev
VERSION = 0.01
NGINX_SITES_AVAILABLE=$(DESTDIR)/etc/nginx/sites-available

build:
	/bin/true

clean:
	/bin/true

force:

install:
	@mkdir -p $(NGINX_SITES_AVAILABLE)
	cp config/nginx/kanaloa $(NGINX_SITES_AVAILABLE)/

package: force
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
