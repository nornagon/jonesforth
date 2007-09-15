# $Id: Makefile,v 1.2 2007-09-15 11:21:09 rich Exp $

all:
	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o jonesforth jonesforth.S

run:
	./jonesforth
