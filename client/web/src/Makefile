OUT_DIR := ../out
DOC_DIR := ../doc

JS_SOURCES := $(wildcard *.js)
JS_OUT := $(JS_SOURCES:%=$(OUT_DIR)/%)
JS_OUT_MIN := $(JS_SOURCES:%.js=$(OUT_DIR)/%.min.js)
OUT_FILES := $(JS_OUT) $(JS_OUT_MIN)

all: $(OUT_FILES)

clean:
	rm -rf $(OUT_DIR)
	rm -rf $(DOC_DIR)

doc:
	jsdoc -d=$(DOC_DIR) -a kanaloa.js

$(OUT_DIR)/%.min.js: %.js
	yuicompressor -o $(OUT_DIR)/`basename $< .js`.min.js $<

$(OUT_DIR)/%.js: %.js
	cp $< $(OUT_DIR)

