NAME = kanaloa
VERSION = 0.01
DEST = $(DESTDIR)/usr/lib/erlang/lib/$(NAME)-$(VERSION)
DEST_EBIN = $(DEST)/ebin
DEST_INCLUDE = $(DEST)/include
DEST_DOC=$(DESTDIR)/usr/share/doc/$(NAME)/html

all: ebin/
	(cd src;$(MAKE) all)
	(cd test;$(MAKE) DEBUG=-DDEBUG)

force:

doc: force
	(cd src;$(MAKE) edoc)

test: force
	(cd src;$(MAKE) test)

clean:
	#(cd src;$(MAKE) clean)
	#(cd test;$(MAKE) clean)
	rm -rf ebin
	rm -rf doc
	rm -rf _test

clean_plt:
	(cd src;$(MAKE) clean_plt)

dialyzer:
	(cd src;$(MAKE) dialyzer)

ebin/:
	@mkdir -p ebin

install:
	mkdir -p $(DEST_EBIN) || true
	mkdir -p $(DEST_INCLUDE) || true
	cp ebin/* $(DEST_EBIN)
	cp src/*.hrl $(DEST_INCLUDE) || true
	mkdir -p $(DEST_DOC) || true
	cp doc/* $(DEST_DOC) || true

package: force
#	Increment the version number
	dch  --newversion $(VERSION)-1-`date +"%Y%m%d%H%M%S"` "Local build"
	dpkg-buildpackage -rfakeroot -b -uc -tc
