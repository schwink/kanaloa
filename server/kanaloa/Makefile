NAME = kanaloa
VERSION = 0.01
DEST = $(DESTDIR)/usr/lib/erlang/lib/$(NAME)-$(VERSION)
DEST_EBIN = $(DEST)/ebin
DEST_INCLUDE = $(DEST)/include
DEST_DOC=$(DESTDIR)/usr/share/doc/$(NAME)/$(VERSION)/html

all: ebin/ tbin/
	(cd src;$(MAKE) all)
	(cd test;$(MAKE) all)

# This requires the erlang-base-hipe package.
hipe: ebin/
	(cd src;$(MAKE) all native=true)

doc:
	(cd src;$(MAKE) edoc)

force:

test: force
	(cd src;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)
	(cd test;$(MAKE) clean)

clean_plt:
	(cd src;$(MAKE) clean_plt)

dialyzer:
	(cd src;$(MAKE) dialyzer)

ebin/:
	@mkdir -p ebin

tbin/:
	@mkdir -p tbin

install:
	@mkdir -p $(DEST_EBIN) || true
	cp ebin/* $(DEST_EBIN)
	@mkdir -p $(DEST_INCLUDE) || true
	cp include/*.hrl $(DEST_INCLUDE)
	@mkdir -p $(DEST_DOC) || true
	cp doc/* $(DEST_DOC) || true

deb:
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
