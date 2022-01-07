# $Id: Makefile,v 1.9 2007-10-22 18:53:12 rich Exp $

#BUILD_ID_NONE := -Wl,--build-id=none
BUILD_ID_NONE :=

SHELL := /bin/bash

DOCKER_IMAGE := my-gcc:11

define build_image
@docker images $(DOCKER_IMAGE) \
	| grep . > /dev/null \
	|| docker build -f Dockerfile.gcc -t $(DOCKER_IMAGE) .
endef

define in_container
$(build_image)
docker run --rm -v $$(PWD):/app $(DOCKER_IMAGE)
endef

define in_container_with_tty
$(build_image)
docker run --rm -it -v $$(PWD):/app $(DOCKER_IMAGE) bash -c
endef

all:	jonesforth

jonesforth: jonesforth.S
	$(in_container) gcc -m32 -nostdlib -static $(BUILD_ID_NONE) -o $@ $<

run:
	$(in_container_with_tty) "cat jonesforth.f $(PROG) - | ./jonesforth"

clean:
	$(in_container) rm -f jonesforth perf_dupdrop *~ core .test_*

# Tests.

TESTS	:= $(patsubst %.f,%.test,$(wildcard test_*.f))

test check: $(TESTS)

test_%.test: test_%.f jonesforth
	@echo -n "$< ... "
	@rm -f .$@
	@$(in_container_with_tty) "cat <(echo ': TEST-MODE ;') jonesforth.f $< <(echo 'TEST') | \
	  ./jonesforth 2>&1 | \
	  sed 's/DSP=[0-9]*//g' > .$@"
	@diff -u .$@ $<.out
	@rm -f .$@
	@echo "ok"

# Performance.

perf_dupdrop: perf_dupdrop.c
	gcc -O3 -Wall -Werror -o $@ $<

run_perf_dupdrop: jonesforth
	@$(in_container_with_tty) "cat <(echo ': TEST-MODE ;') jonesforth.f perf_dupdrop.f | ./jonesforth"

.SUFFIXES: .f .test
.PHONY: test check run run_perf_dupdrop
