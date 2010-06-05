all: ebin/
	(cd src;$(MAKE) all)
	(cd test;$(MAKE) DEBUG=-DDEBUG)

force:

edoc: force
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
