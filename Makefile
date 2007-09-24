# $Id: Makefile,v 1.3 2007-09-24 00:18:19 rich Exp $

all:
	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o jonesforth jonesforth.S

run:
	cat jonesforth.f - | ./jonesforth

