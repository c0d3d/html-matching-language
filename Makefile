RACO=raco
SRC=lib test
JOBS := $(shell nproc 2> /dev/null)
PROJ_NAME := $(shell basename $(shell pwd))


ifndef JOBS
	JOBS := 4 # Safe to assume they have at least 4
else
	JOBS := --jobs $(JOBS)
endif

TEST := test $(JOBS)
PKG_INSTALL := pkg install --batch $(JOBS)
PKG_REMOVE  := pkg remove --batch $(JOBS)

test all: .setup
	$(RACO) $(TEST) $(SRC)

.setup:
	$(RACO) $(PKG_INSTALL) --deps search-auto || exit 0
	@touch .setup

install: test

clean:
	find . -name "compiled" -type d -prune -exec rm -r {} \;

uninstall: clean
	$(RACO) $(PKG_REMOVE) $(PROJ_NAME)
	-rm -rf .setup

.PHONY: all clean install test uninstall
