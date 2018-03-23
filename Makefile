RACO=raco
SRC=lib test
JOBS := $(shell nproc 2> /dev/null)
TEST=test --jobs
PROJ_NAME := $(shell basename $(shell pwd))


ifndef JOBS
	TEST += 4 # Safe to assume they have at least 4
else
	TEST += $(JOBS)
endif

test all: .setup
	$(RACO) $(TEST) $(SRC)

.setup:
	$(RACO) pkg install || exit 0
	@touch .setup

install: test

clean:
	find . -name "compiled" -type d -prune -exec rm -r {} \;

uninstall:
	$(RACO) pkg remove $(PROJ_NAME)
	-rm -rf .setup

.PHONY: all clean install test uninstall
