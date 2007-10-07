# $Id: Makefile,v 1.6 2007-10-07 11:07:15 rich Exp $

SHELL	:= /bin/bash

all:	jonesforth

jonesforth: jonesforth.S
	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -Wl,--build-id=none -o $@ $<

run:
	cat jonesforth.f $(PROG) - | ./jonesforth

clean:
	rm -f jonesforth *~ core .test_*

TESTS	:= $(patsubst %.f,%.test,$(wildcard test_*.f))

test check: $(TESTS)

test_%.test: test_%.f jonesforth
	@echo -n "$< ... "
	@rm -f .$@
	@cat <(echo ': TEST-MODE ;') jonesforth.f $< <(echo 'TEST') | \
	  ./jonesforth 2>&1 | \
	  sed 's/DSP=[0-9]*//g' > .$@
	@diff -u .$@ $<.out
	@rm -f .$@
	@echo "ok"

.SUFFIXES: .f .test
.PHONY: test check

remote:
	scp jonesforth.S jonesforth.f rjones@oirase:Desktop/
	ssh rjones@oirase sh -c '"rm -f Desktop/jonesforth; \
	  gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o Desktop/jonesforth Desktop/jonesforth.S; \
	  cat Desktop/jonesforth.f - | Desktop/jonesforth arg1 arg2 arg3"'
