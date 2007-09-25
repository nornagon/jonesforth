# $Id: Makefile,v 1.4 2007-09-25 21:45:26 rich Exp $

all:
	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o jonesforth jonesforth.S

run:
	cat jonesforth.f - | ./jonesforth

remote:
	scp jonesforth.S jonesforth.f rjones@oirase:Desktop/
	ssh rjones@oirase sh -c '"rm -f Desktop/jonesforth; \
	  gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o Desktop/jonesforth Desktop/jonesforth.S; \
	  cat Desktop/jonesforth.f - | Desktop/jonesforth"'
