# $Id: Makefile,v 1.1 2007-09-07 23:40:52 rich Exp $

all:
	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o jonesforth jonesforth.S

run:
	./jonesforth