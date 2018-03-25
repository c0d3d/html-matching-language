RACO=raco
SRC=lib test
JOBS := $(shell nproc 2> /dev/null)
PROJ_NAME := $(shell basename $(shell pwd))
SCRIBBLINGS_DIR := scribblings
DOC_DIR := doc
BASE_DOC := hml-lang.scrbl
DOC_OUTPUT := $(DOC_DIR)/hml-lang


ifndef JOBS
	JOBS := 4 # Safe to assume they have at least 4
else
	JOBS := --jobs $(JOBS)
endif

TEST := test $(JOBS)
PKG_INSTALL := pkg install --batch $(JOBS)
PKG_REMOVE  := pkg remove --batch $(JOBS)
SCRIBBLE := scribble --htmls

test: .setup
	$(RACO) $(TEST) $(SRC)

$(DOC_OUTPUT)/index.html:
	 cd $(SCRIBBLINGS_DIR); $(RACO) $(SCRIBBLE) --dest $(DOC_DIR) $(BASE_DOC)

.setup:
	$(RACO) $(PKG_INSTALL) --deps search-auto || exit 0
	@touch .setup

clean:
	find . -name "compiled" -type d -prune -exec rm -r {} \;
	-rm -rf $(DOC_OUTPUT)

install: test
all: test
docs: $(DOC_OUTPUT)/index.html

uninstall: clean
	$(RACO) $(PKG_REMOVE) $(PROJ_NAME)
	-rm -rf .setup

.PHONY: all clean install test uninstall docs
