RACO=raco
SRC=lib test
JOBS := $(shell nproc 2> /dev/null)
TEST=test --jobs
ifndef JOBS
	TEST += 4 # Safe to assume they have at least 4
else
	TEST += $(JOBS)
endif

all:
	$(RACO) $(TEST) $(SRC)

clean:
	find . -name "compiled" -type d -prune -exec rm -r {} \;

.PHONY: all clean
