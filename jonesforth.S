/*	A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems. -*- asm -*-
	By Richard W.M. Jones <rich@annexia.org> http://annexia.org/forth
	This is PUBLIC DOMAIN (see public domain release statement below).
	$Id: jonesforth.S,v 1.27 2007-09-24 00:01:38 rich Exp $

	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o jonesforth jonesforth.S
*/
	.set JONES_VERSION,27
/*
	INTRODUCTION ----------------------------------------------------------------------

	FORTH is one of those alien languages which most working programmers regard in the same
	way as Haskell, LISP, and so on.  Something so strange that they'd rather any thoughts
	of it just go away so they can get on with writing this paying code.  But that's wrong
	and if you care at all about programming then you should at least understand all these
	languages, even if you will never use them.

	LISP is the ultimate high-level language, and features from LISP are being added every
	decade to the more common languages.  But FORTH is in some ways the ultimate in low level
	programming.  Out of the box it lacks features like dynamic memory management and even
	strings.  In fact, at its primitive level it lacks even basic concepts like IF-statements
	and loops.

	Why then would you want to learn FORTH?  There are several very good reasons.  First
	and foremost, FORTH is minimal.  You really can write a complete FORTH in, say, 2000
	lines of code.  I don't just mean a FORTH program, I mean a complete FORTH operating
	system, environment and language.  You could boot such a FORTH on a bare PC and it would
	come up with a prompt where you could start doing useful work.  The FORTH you have here
	isn't minimal and uses a Linux process as its 'base PC' (both for the purposes of making
	it a good tutorial). It's possible to completely understand the system.  Who can say they
	completely understand how Linux works, or gcc?

	Secondly FORTH has a peculiar bootstrapping property.  By that I mean that after writing
	a little bit of assembly to talk to the hardware and implement a few primitives, all the
	rest of the language and compiler is written in FORTH itself.  Remember I said before
	that FORTH lacked IF-statements and loops?  Well of course it doesn't really because
	such a lanuage would be useless, but my point was rather that IF-statements and loops are
	written in FORTH itself.

	Now of course this is common in other languages as well, and in those languages we call
	them 'libraries'.  For example in C, 'printf' is a library function written in C.  But
	in FORTH this goes way beyond mere libraries.  Can you imagine writing C's 'if' in C?
	And that brings me to my third reason: If you can write 'if' in FORTH, then why restrict
	yourself to the usual if/while/for/switch constructs?  You want a construct that iterates
	over every other element in a list of numbers?  You can add it to the language.  What
	about an operator which pulls in variables directly from a configuration file and makes
	them available as FORTH variables?  Or how about adding Makefile-like dependencies to
	the language?  No problem in FORTH.  This concept isn't common in programming languages,
	but it has a name (in fact two names): "macros" (by which I mean LISP-style macros, not
	the lame C preprocessor) and "domain specific languages" (DSLs).

	This tutorial isn't about learning FORTH as the language.  I'll point you to some references
	you should read if you're not familiar with using FORTH.  This tutorial is about how to
	write FORTH.  In fact, until you understand how FORTH is written, you'll have only a very
	superficial understanding of how to use it.

	So if you're not familiar with FORTH or want to refresh your memory here are some online
	references to read:

	http://en.wikipedia.org/wiki/Forth_%28programming_language%29

	http://galileo.phys.virginia.edu/classes/551.jvn.fall01/primer.htm

	http://wiki.laptop.org/go/Forth_Lessons

	http://www.albany.net/~hello/simple.htm

	Here is another "Why FORTH?" essay: http://www.jwdt.com/~paysan/why-forth.html

	Discussion and criticism of this FORTH here: http://lambda-the-ultimate.org/node/2452

	ACKNOWLEDGEMENTS ----------------------------------------------------------------------

	This code draws heavily on the design of LINA FORTH (http://home.hccnet.nl/a.w.m.van.der.horst/lina.html)
	by Albert van der Horst.  Any similarities in the code are probably not accidental.

	Also I used this document (http://ftp.funet.fi/pub/doc/IOCCC/1992/buzzard.2.design) which really
	defies easy explanation.

	PUBLIC DOMAIN ----------------------------------------------------------------------

	I, the copyright holder of this work, hereby release it into the public domain. This applies worldwide.

	In case this is not legally possible, I grant any entity the right to use this work for any purpose,
	without any conditions, unless such conditions are required by law.

	SETTING UP ----------------------------------------------------------------------

	Let's get a few housekeeping things out of the way.  Firstly because I need to draw lots of
	ASCII-art diagrams to explain concepts, the best way to look at this is using a window which
	uses a fixed width font and is at least this wide:

 <------------------------------------------------------------------------------------------------------------------------>

	Secondly make sure TABS are set to 8 characters.  The following should be a vertical
	line.  If not, sort out your tabs.

	|
        |
	|

	Thirdly I assume that your screen is at least 50 characters high.

	ASSEMBLING ----------------------------------------------------------------------

	If you want to actually run this FORTH, rather than just read it, you will need Linux on an
	i386.  Linux because instead of programming directly to the hardware on a bare PC which I
	could have done, I went for a simpler tutorial by assuming that the 'hardware' is a Linux
	process with a few basic system calls (read, write and exit and that's about all).  i386
	is needed because I had to write the assembly for a processor, and i386 is by far the most
	common.  (Of course when I say 'i386', any 32- or 64-bit x86 processor will do.  I'm compiling
	this on a 64 bit AMD Opteron).

	Again, to assemble this you will need gcc and gas (the GNU assembler).  The commands to
	assemble and run the code (save this file as 'jonesforth.S') are:

	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o jonesforth jonesforth.S
	./jonesforth

	You will see lots of 'Warning: unterminated string; newline inserted' messages from the
	assembler.  That's just because the GNU assembler doesn't have a good syntax for multi-line
	strings (or rather it used to, but the developers removed it!) so I've abused the syntax
	slightly to make things readable.  Ignore these warnings.

	If you want to run your own FORTH programs you can do:

	./jonesforth < myprog.f

	If you want to load your own FORTH code and then continue reading user commands, you can do:

	cat myfunctions.f - | ./jonesforth

	ASSEMBLER ----------------------------------------------------------------------

	(You can just skip to the next section -- you don't need to be able to read assembler to
	follow this tutorial).

	However if you do want to read the assembly code here are a few notes about gas (the GNU assembler):

	(1) Register names are prefixed with '%', so %eax is the 32 bit i386 accumulator.  The registers
	    available on i386 are: %eax, %ebx, %ecx, %edx, %esi, %edi, %ebp and %esp, and most of them
	    have special purposes.

	(2) Add, mov, etc. take arguments in the form SRC,DEST.  So mov %eax,%ecx moves %eax -> %ecx

	(3) Constants are prefixed with '$', and you mustn't forget it!  If you forget it then it
	    causes a read from memory instead, so:
	    mov $2,%eax		moves number 2 into %eax
	    mov 2,%eax		reads the 32 bit word from address 2 into %eax (ie. most likely a mistake)

	(4) gas has a funky syntax for local labels, where '1f' (etc.) means label '1:' "forwards"
	    and '1b' (etc.) means label '1:' "backwards".

	(5) 'ja' is "jump if above", 'jb' for "jump if below", 'je' "jump if equal" etc.

	(6) gas has a reasonably nice .macro syntax, and I use them a lot to make the code shorter and
	    less repetitive.

	For more help reading the assembler, do "info gas" at the Linux prompt.

	Now the tutorial starts in earnest.

	THE DICTIONARY ----------------------------------------------------------------------

	In FORTH as you will know, functions are called "words", and just as in other languages they
	have a name and a definition.  Here are two FORTH words:

	: DOUBLE DUP + ;		\ name is "DOUBLE", definition is "DUP +"
	: QUADRUPLE DOUBLE DOUBLE ;	\ name is "QUADRUPLE", definition is "DOUBLE DOUBLE"

	Words, both built-in ones and ones which the programmer defines later, are stored in a dictionary
	which is just a linked list of dictionary entries.

	<--- DICTIONARY ENTRY (HEADER) ----------------------->
	+------------------------+--------+---------- - - - - +----------- - - - -
	| LINK POINTER           | LENGTH/| NAME	      | DEFINITION
	|			 | FLAGS  |     	      |
	+--- (4 bytes) ----------+- byte -+- n bytes  - - - - +----------- - - - -

	I'll come to the definition of the word later.  For now just look at the header.  The first
	4 bytes are the link pointer.  This points back to the previous word in the dictionary, or, for
	the first word in the dictionary it is just a NULL pointer.  Then comes a length/flags byte.
	The length of the word can be up to 31 characters (5 bits used) and the top three bits are used
	for various flags which I'll come to later.  This is followed by the name itself, and in this
	implementation the name is rounded up to a multiple of 4 bytes by padding it with zero bytes.
	That's just to ensure that the definition starts on a 32 bit boundary.

	A FORTH variable called LATEST contains a pointer to the most recently defined word, in
	other words, the head of this linked list.

	DOUBLE and QUADRUPLE might look like this:

	  pointer to previous word
	   ^
	   |
	+--|------+---+---+---+---+---+---+---+---+------------- - - - -
	| LINK    | 6 | D | O | U | B | L | E | 0 | (definition ...)
	+---------+---+---+---+---+---+---+---+---+------------- - - - -
           ^       len                         padding
	   |
	+--|------+---+---+---+---+---+---+---+---+---+---+---+---+------------- - - - -
	| LINK    | 9 | Q | U | A | D | R | U | P | L | E | 0 | 0 | (definition ...)
	+---------+---+---+---+---+---+---+---+---+---+---+---+---+------------- - - - -
           ^       len                                     padding
           |
           |
	  LATEST

	You should be able to see from this how you might implement functions to find a word in
	the dictionary (just walk along the dictionary entries starting at LATEST and matching
	the names until you either find a match or hit the NULL pointer at the end of the dictionary);
	and add a word to the dictionary (create a new definition, set its LINK to LATEST, and set
	LATEST to point to the new word).  We'll see precisely these functions implemented in
	assembly code later on.

	One interesting consequence of using a linked list is that you can redefine words, and
	a newer definition of a word overrides an older one.  This is an important concept in
	FORTH because it means that any word (even "built-in" or "standard" words) can be
	overridden with a new definition, either to enhance it, to make it faster or even to
	disable it.  However because of the way that FORTH words get compiled, which you'll
	understand below, words defined using the old definition of a word continue to use
	the old definition.  Only words defined after the new definition use the new definition.

	DIRECT THREADED CODE ----------------------------------------------------------------------

	Now we'll get to the really crucial bit in understanding FORTH, so go and get a cup of tea
	or coffee and settle down.  It's fair to say that if you don't understand this section, then you
	won't "get" how FORTH works, and that would be a failure on my part for not explaining it well.
	So if after reading this section a few times you don't understand it, please email me
	(rich@annexia.org).

	Let's talk first about what "threaded code" means.  Imagine a peculiar version of C where
	you are only allowed to call functions without arguments.  (Don't worry for now that such a
	language would be completely useless!)  So in our peculiar C, code would look like this:

	f ()
	{
	  a ();
	  b ();
	  c ();
	}

	and so on.  How would a function, say 'f' above, be compiled by a standard C compiler?
	Probably into assembly code like this.  On the right hand side I've written the actual
	i386 machine code.

	f:
	  CALL a			E8 08 00 00 00
	  CALL b			E8 1C 00 00 00
	  CALL c			E8 2C 00 00 00
	  ; ignore the return from the function for now

	"E8" is the x86 machine code to "CALL" a function.  In the first 20 years of computing
	memory was hideously expensive and we might have worried about the wasted space being used
	by the repeated "E8" bytes.  We can save 20% in code size (and therefore, in expensive memory)
	by compressing this into just:

	08 00 00 00		Just the function addresses, without
	1C 00 00 00		the CALL prefix.
	2C 00 00 00

	On a 16-bit machine like the ones which originally ran FORTH the savings are even greater - 33%.

	[Historical note: If the execution model that FORTH uses looks strange from the following
	paragraphs, then it was motivated entirely by the need to save memory on early computers.
	This code compression isn't so important now when our machines have more memory in their L1
	caches than those early computers had in total, but the execution model still has some
	useful properties].

	Of course this code won't run directly any more.  Instead we need to write an interpreter
	which takes each pair of bytes and calls it.

	On an i386 machine it turns out that we can write this interpreter rather easily, in just
	two assembly instructions which turn into just 3 bytes of machine code.  Let's store the
	pointer to the next word to execute in the %esi register:

		08 00 00 00	<- We're executing this one now.  %esi is the _next_ one to execute.
	%esi -> 1C 00 00 00
		2C 00 00 00

	The all-important i386 instruction is called LODSL (or in Intel manuals, LODSW).  It does
	two things.  Firstly it reads the memory at %esi into the accumulator (%eax).  Secondly it
	increments %esi by 4 bytes.  So after LODSL, the situation now looks like this:

		08 00 00 00	<- We're still executing this one
		1C 00 00 00	<- %eax now contains this address (0x0000001C)
	%esi -> 2C 00 00 00

	Now we just need to jump to the address in %eax.  This is again just a single x86 instruction
	written JMP *(%eax).  And after doing the jump, the situation looks like:

		08 00 00 00
		1C 00 00 00	<- Now we're executing this subroutine.
	%esi -> 2C 00 00 00

	To make this work, each subroutine is followed by the two instructions 'LODSL; JMP *(%eax)'
	which literally make the jump to the next subroutine.

	And that brings us to our first piece of actual code!  Well, it's a macro.
*/

/* NEXT macro. */
	.macro NEXT
	lodsl
	jmp *(%eax)
	.endm

/*	The macro is called NEXT.  That's a FORTH-ism.  It expands to those two instructions.

	Every FORTH primitive that we write has to be ended by NEXT.  Think of it kind of like
	a return.

	The above describes what is known as direct threaded code.

	To sum up: We compress our function calls down to a list of addresses and use a somewhat
	magical macro to act as a "jump to next function in the list".  We also use one register (%esi)
	to act as a kind of instruction pointer, pointing to the next function in the list.

	I'll just give you a hint of what is to come by saying that a FORTH definition such as:

	: QUADRUPLE DOUBLE DOUBLE ;

	actually compiles (almost, not precisely but we'll see why in a moment) to a list of
	function addresses for DOUBLE, DOUBLE and a special function called EXIT to finish off.

	At this point, REALLY EAGLE-EYED ASSEMBLY EXPERTS are saying "JONES, YOU'VE MADE A MISTAKE!".

	I lied about JMP *(%eax).  

	INDIRECT THREADED CODE ----------------------------------------------------------------------

	It turns out that direct threaded code is interesting but only if you want to just execute
	a list of functions written in assembly language.  So QUADRUPLE would work only if DOUBLE
	was an assembly language function.  In the direct threaded code, QUADRUPLE would look like:

		+------------------+
		| addr of DOUBLE  --------------------> (assembly code to do the double)
		+------------------+                    NEXT
	%esi ->	| addr of DOUBLE   |
		+------------------+

	We can add an extra indirection to allow us to run both words written in assembly language
	(primitives written for speed) and words written in FORTH themselves as lists of addresses.

	The extra indirection is the reason for the brackets in JMP *(%eax).

	Let's have a look at how QUADRUPLE and DOUBLE really look in FORTH:

	        : QUADRUPLE DOUBLE DOUBLE ;

		+------------------+
		| codeword         |		   : DOUBLE DUP + ;
		+------------------+
		| addr of DOUBLE  ---------------> +------------------+
		+------------------+               | codeword         |
		| addr of DOUBLE   |		   +------------------+
		+------------------+	   	   | addr of DUP   --------------> +------------------+
		| addr of EXIT	   |		   +------------------+            | codeword      -------+
		+------------------+	   %esi -> | addr of +     --------+	   +------------------+   |
						   +------------------+	   |	   | assembly to    <-----+
						   | addr of EXIT     |    |       | implement DUP    |
						   +------------------+	   |	   |	..	      |
									   |	   |    ..            |
									   |	   | NEXT             |
									   |	   +------------------+
									   |
									   +-----> +------------------+
										   | codeword      -------+
										   +------------------+   |
										   | assembly to   <------+
										   | implement +      |
										   | 	..            |
										   | 	..            |
										   | NEXT      	      |
										   +------------------+

	This is the part where you may need an extra cup of tea/coffee/favourite caffeinated
	beverage.  What has changed is that I've added an extra pointer to the beginning of
	the definitions.  In FORTH this is sometimes called the "codeword".  The codeword is
	a pointer to the interpreter to run the function.  For primitives written in
	assembly language, the "interpreter" just points to the actual assembly code itself.
	They don't need interpreting, they just run.

	In words written in FORTH (like QUADRUPLE and DOUBLE), the codeword points to an interpreter
	function.

	I'll show you the interpreter function shortly, but let's recall our indirect
	JMP *(%eax) with the "extra" brackets.  Take the case where we're executing DOUBLE
	as shown, and DUP has been called.  Note that %esi is pointing to the address of +

	The assembly code for DUP eventually does a NEXT.  That:

	(1) reads the address of + into %eax		%eax points to the codeword of +
	(2) increments %esi by 4
	(3) jumps to the indirect %eax			jumps to the address in the codeword of +,
							ie. the assembly code to implement +

		+------------------+
		| codeword         |
		+------------------+
		| addr of DOUBLE  ---------------> +------------------+
		+------------------+               | codeword         |
		| addr of DOUBLE   |		   +------------------+
		+------------------+	   	   | addr of DUP   --------------> +------------------+
		| addr of EXIT	   |		   +------------------+            | codeword      -------+
		+------------------+	   	   | addr of +     --------+	   +------------------+   |
						   +------------------+	   |	   | assembly to    <-----+
					   %esi -> | addr of EXIT     |    |       | implement DUP    |
						   +------------------+	   |	   |	..	      |
									   |	   |    ..            |
									   |	   | NEXT             |
									   |	   +------------------+
									   |
									   +-----> +------------------+
										   | codeword      -------+
										   +------------------+   |
									now we're  | assembly to    <-----+
									executing  | implement +      |
									this	   | 	..            |
									function   | 	..            |
										   | NEXT      	      |
										   +------------------+

	So I hope that I've convinced you that NEXT does roughly what you'd expect.  This is
	indirect threaded code.

	I've glossed over four things.  I wonder if you can guess without reading on what they are?

	.
	.
	.

	My list of four things are: (1) What does "EXIT" do?  (2) which is related to (1) is how do
	you call into a function, ie. how does %esi start off pointing at part of QUADRUPLE, but
	then point at part of DOUBLE.  (3) What goes in the codeword for the words which are written
	in FORTH?  (4) How do you compile a function which does anything except call other functions
	ie. a function which contains a number like : DOUBLE 2 * ; ?

	THE INTERPRETER AND RETURN STACK ------------------------------------------------------------

	Going at these in no particular order, let's talk about issues (3) and (2), the interpreter
	and the return stack.

	Words which are defined in FORTH need a codeword which points to a little bit of code to
	give them a "helping hand" in life.  They don't need much, but they do need what is known
	as an "interpreter", although it doesn't really "interpret" in the same way that, say,
	Java bytecode used to be interpreted (ie. slowly).  This interpreter just sets up a few
	machine registers so that the word can then execute at full speed using the indirect
	threaded model above.

	One of the things that needs to happen when QUADRUPLE calls DOUBLE is that we save the old
	%esi ("instruction pointer") and create a new one pointing to the first word in DOUBLE.
	Because we will need to restore the old %esi at the end of DOUBLE (this is, after all, like
	a function call), we will need a stack to store these "return addresses" (old values of %esi).

	As you will have read, when reading the background documentation, FORTH has two stacks,
	an ordinary stack for parameters, and a return stack which is a bit more mysterious.  But
	our return stack is just the stack I talked about in the previous paragraph, used to save
	%esi when calling from a FORTH word into another FORTH word.

	In this FORTH, we are using the normal stack pointer (%esp) for the parameter stack.
	We will use the i386's "other" stack pointer (%ebp, usually called the "frame pointer")
	for our return stack.

	I've got two macros which just wrap up the details of using %ebp for the return stack.
	You use them as for example "PUSHRSP %eax" (push %eax on the return stack) or "POPRSP %ebx"
	(pop top of return stack into %ebx).
*/

/* Macros to deal with the return stack. */
	.macro PUSHRSP reg
	lea -4(%ebp),%ebp	// push reg on to return stack
	movl \reg,(%ebp)
	.endm

	.macro POPRSP reg
	mov (%ebp),\reg		// pop top of return stack to reg
	lea 4(%ebp),%ebp
	.endm

/*
	And with that we can now talk about the interpreter.

	In FORTH the interpreter function is often called DOCOL (I think it means "DO COLON" because
	all FORTH definitions start with a colon, as in : DOUBLE DUP + ;

	The "interpreter" (it's not really "interpreting") just needs to push the old %esi on the
	stack and set %esi to the first word in the definition.  Remember that we jumped to the
	function using JMP *(%eax)?  Well a consequence of that is that conveniently %eax contains
	the address of this codeword, so just by adding 4 to it we get the address of the first
	data word.  Finally after setting up %esi, it just does NEXT which causes that first word
	to run.
*/

/* DOCOL - the interpreter! */
	.text
	.align 4
DOCOL:
	PUSHRSP %esi		// push %esi on to the return stack
	addl $4,%eax		// %eax points to codeword, so make
	movl %eax,%esi		// %esi point to first data word
	NEXT

/*
	Just to make this absolutely clear, let's see how DOCOL works when jumping from QUADRUPLE
	into DOUBLE:

		QUADRUPLE:
		+------------------+
		| codeword         |
		+------------------+		   DOUBLE:
		| addr of DOUBLE  ---------------> +------------------+
		+------------------+       %eax -> | addr of DOCOL    |
	%esi ->	| addr of DOUBLE   |		   +------------------+
		+------------------+	   	   | addr of DUP      |
		| addr of EXIT	   |		   +------------------+
		+------------------+               | etc.             |

	First, the call to DOUBLE calls DOCOL (the codeword of DOUBLE).  DOCOL does this:  It
	pushes the old %esi on the return stack.  %eax points to the codeword of DOUBLE, so we
	just add 4 on to it to get our new %esi:

		QUADRUPLE:
		+------------------+
		| codeword         |
		+------------------+		   DOUBLE:
		| addr of DOUBLE  ---------------> +------------------+
top of return	+------------------+       %eax -> | addr of DOCOL    |
stack points ->	| addr of DOUBLE   |	   + 4 =   +------------------+
		+------------------+	   %esi -> | addr of DUP      |
		| addr of EXIT	   |		   +------------------+
		+------------------+               | etc.             |

	Then we do NEXT, and because of the magic of threaded code that increments %esi again
	and calls DUP.

	Well, it seems to work.

	One minor point here.  Because DOCOL is the first bit of assembly actually to be defined
	in this file (the others were just macros), and because I usually compile this code with the
	text segment starting at address 0, DOCOL has address 0.  So if you are disassembling the
	code and see a word with a codeword of 0, you will immediately know that the word is
	written in FORTH (it's not an assembler primitive) and so uses DOCOL as the interpreter.

	STARTING UP ----------------------------------------------------------------------

	Now let's get down to nuts and bolts.  When we start the program we need to set up
	a few things like the return stack.  But as soon as we can, we want to jump into FORTH
	code (albeit much of the "early" FORTH code will still need to be written as
	assembly language primitives).

	This is what the set up code does.  Does a tiny bit of house-keeping, sets up the
	separate return stack (NB: Linux gives us the ordinary parameter stack already), then
	immediately jumps to a FORTH word called COLD.  COLD stands for cold-start.  In ISO
	FORTH (but not in this FORTH), COLD can be called at any time to completely reset
	the state of FORTH, and there is another word called WARM which does a partial reset.
*/

/* ELF entry point. */
	.text
	.globl _start
_start:
	cld
	mov %esp,var_S0		// Store the initial data stack pointer.
	mov $return_stack,%ebp	// Initialise the return stack.

	mov $cold_start,%esi	// Initialise interpreter.
	NEXT			// Run interpreter!

	.section .rodata
cold_start:			// High-level code without a codeword.
	.int COLD

/*
	We also allocate some space for the return stack and some space to store user
	definitions.  These are static memory allocations using fixed-size buffers, but it
	wouldn't be a great deal of work to make them dynamic.
*/

	.bss
/* FORTH return stack. */
	.set RETURN_STACK_SIZE,8192
	.align 4096
	.space RETURN_STACK_SIZE
return_stack:			// Initial top of return stack.

/* The user definitions area: space for user-defined words and general memory allocations. */
	.set USER_DEFS_SIZE,65536
	.align 4096
user_defs_start:
	.space USER_DEFS_SIZE

/*
	BUILT-IN WORDS ----------------------------------------------------------------------

	Remember our dictionary entries (headers).  Let's bring those together with the codeword
	and data words to see how : DOUBLE DUP + ; really looks in memory.

	  pointer to previous word
	   ^
	   |
	+--|------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
	+---------+---+---+---+---+---+---+---+---+------------+--|---------+------------+------------+
           ^       len                         pad  codeword      |
	   |							  V
	  LINK in next word				points to codeword of DUP
	
	Initially we can't just write ": DOUBLE DUP + ;" (ie. that literal string) here because we
	don't yet have anything to read the string, break it up at spaces, parse each word, etc. etc.
	So instead we will have to define built-in words using the GNU assembler data constructors
	(like .int, .byte, .string, .ascii and so on -- look them up in the gas info page if you are
	unsure of them).

	The long way would be:
	.int <link to previous word>
	.byte 6			// len
	.ascii "DOUBLE"		// string
	.byte 0			// padding
DOUBLE: .int DOCOL		// codeword
	.int DUP		// pointer to codeword of DUP
	.int PLUS		// pointer to codeword of +
	.int EXIT		// pointer to codeword of EXIT

	That's going to get quite tedious rather quickly, so here I define an assembler macro
	so that I can just write:

	defword "DOUBLE",6,,DOUBLE
	.int DUP,PLUS,EXIT

	and I'll get exactly the same effect.

	Don't worry too much about the exact implementation details of this macro - it's complicated!
*/

/* Flags - these are discussed later. */
	.set F_IMMED,0x80
	.set F_HIDDEN,0x20
	.set F_LENMASK,0x1f	// length mask

	// Store the chain of links.
	.set link,0

	.macro defword name, namelen, flags=0, label
	.section .rodata
	.align 4
	.globl name_\label
name_\label :
	.int link		// link
	.set link,name_\label
	.byte \flags+\namelen	// flags + length byte
	.ascii "\name"		// the name
	.align 4
	.globl \label
\label :
	.int DOCOL		// codeword - the interpreter
	// list of word pointers follow
	.endm

/*
	Similarly I want a way to write words written in assembly language.  There will quite a few
	of these to start with because, well, everything has to start in assembly before there's
	enough "infrastructure" to be able to start writing FORTH words, but also I want to define
	some common FORTH words in assembly language for speed, even though I could write them in FORTH.

	This is what DUP looks like in memory:

	  pointer to previous word
	   ^
	   |
	+--|------+---+---+---+---+------------+
	| LINK    | 3 | D | U | P | code_DUP ---------------------> points to the assembly
	+---------+---+---+---+---+------------+		    code used to write DUP,
           ^       len              codeword			    which ends with NEXT.
	   |
	  LINK in next word

	Again, for brevity in writing the header I'm going to write an assembler macro called defcode.
*/

	.macro defcode name, namelen, flags=0, label
	.section .rodata
	.align 4
	.globl name_\label
name_\label :
	.int link		// link
	.set link,name_\label
	.byte \flags+\namelen	// flags + length byte
	.ascii "\name"		// the name
	.align 4
	.globl \label
\label :
	.int code_\label	// codeword
	.text
	.align 4
	.globl code_\label
code_\label :			// assembler code follows
	.endm

/*
	Now some easy FORTH primitives.  These are written in assembly for speed.  If you understand
	i386 assembly language then it is worth reading these.  However if you don't understand assembly
	you can skip the details.
*/

	defcode "DUP",3,,DUP
	pop %eax		// duplicate top of stack
	push %eax
	push %eax
	NEXT

	defcode "DROP",4,,DROP
	pop %eax		// drop top of stack
	NEXT

	defcode "SWAP",4,,SWAP
	pop %eax		// swap top of stack
	pop %ebx
	push %eax
	push %ebx
	NEXT

	defcode "OVER",4,,OVER
	mov 4(%esp),%eax	// get the second element of stack
	push %eax		// and push it on top
	NEXT

	defcode "ROT",3,,ROT
	pop %eax
	pop %ebx
	pop %ecx
	push %eax
	push %ecx
	push %ebx
	NEXT

	defcode "-ROT",4,,NROT
	pop %eax
	pop %ebx
	pop %ecx
	push %ebx
	push %eax
	push %ecx
	NEXT

	defcode "1+",2,,INCR
	incl (%esp)		// increment top of stack
	NEXT

	defcode "1-",2,,DECR
	decl (%esp)		// decrement top of stack
	NEXT

	defcode "4+",2,,INCR4
	addl $4,(%esp)		// add 4 to top of stack
	NEXT

	defcode "4-",2,,DECR4
	subl $4,(%esp)		// subtract 4 from top of stack
	NEXT

	defcode "+",1,,ADD
	pop %eax		// get top of stack
	addl %eax,(%esp)	// and add it to next word on stack
	NEXT

	defcode "-",1,,SUB
	pop %eax		// get top of stack
	subl %eax,(%esp)	// and subtract it from next word on stack
	NEXT

	defcode "*",1,,MUL
	pop %eax
	pop %ebx
	imull %ebx,%eax
	push %eax		// ignore overflow
	NEXT

	defcode "/",1,,DIV
	xor %edx,%edx
	pop %ebx
	pop %eax
	idivl %ebx
	push %eax		// push quotient
	NEXT

	defcode "MOD",3,,MOD
	xor %edx,%edx
	pop %ebx
	pop %eax
	idivl %ebx
	push %edx		// push remainder
	NEXT

	defcode "=",1,,EQU	// top two words are equal?
	pop %eax
	pop %ebx
	cmp %ebx,%eax
	je 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "<>",2,,NEQU	// top two words are not equal?
	pop %eax
	pop %ebx
	cmp %ebx,%eax
	je 1f
	pushl $1
	NEXT
1:	pushl $0
	NEXT

	defcode "<",1,,LT
	pop %eax
	pop %ebx
	cmp %eax,%ebx
	jl 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode ">",1,,GT
	pop %eax
	pop %ebx
	cmp %eax,%ebx
	jg 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "<=",2,,LE
	pop %eax
	pop %ebx
	cmp %eax,%ebx
	jle 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode ">=",2,,GE
	pop %eax
	pop %ebx
	cmp %eax,%ebx
	jge 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "0=",2,,ZEQU	// top of stack equals 0?
	pop %eax
	test %eax,%eax
	jz 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "0<>",3,,ZNEQU	// top of stack not 0?
	pop %eax
	test %eax,%eax
	jnz 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "0<",2,,ZLT
	pop %eax
	test %eax,%eax
	jl 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "0>",2,,ZGT
	pop %eax
	test %eax,%eax
	jg 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "0<=",3,,ZLE
	pop %eax
	test %eax,%eax
	jle 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "0>=",3,,ZGE
	pop %eax
	test %eax,%eax
	jge 1f
	pushl $0
	NEXT
1:	pushl $1
	NEXT

	defcode "AND",3,,AND
	pop %eax
	andl %eax,(%esp)
	NEXT

	defcode "OR",2,,OR
	pop %eax
	orl %eax,(%esp)
	NEXT

	defcode "XOR",3,,XOR
	pop %eax
	xorl %eax,(%esp)
	NEXT

	defcode "INVERT",6,,INVERT // this is the FORTH bitwise "NOT" function
	notl (%esp)
	NEXT

/*
	RETURNING FROM FORTH WORDS ----------------------------------------------------------------------

	Time to talk about what happens when we EXIT a function.  In this diagram QUADRUPLE has called
	DOUBLE, and DOUBLE is about to exit (look at where %esi is pointing):

		QUADRUPLE
		+------------------+
		| codeword         |
		+------------------+		   DOUBLE
		| addr of DOUBLE  ---------------> +------------------+
		+------------------+               | codeword         |
		| addr of DOUBLE   |		   +------------------+
		+------------------+	   	   | addr of DUP      |
		| addr of EXIT	   |		   +------------------+
		+------------------+	   	   | addr of +        |
						   +------------------+
					   %esi -> | addr of EXIT     |
						   +------------------+

	What happens when the + function does NEXT?  Well, the following code is executed.
*/

	defcode "EXIT",4,,EXIT
	POPRSP %esi		// pop return stack into %esi
	NEXT

/*
	EXIT gets the old %esi which we saved from before on the return stack, and puts it in %esi.
	So after this (but just before NEXT) we get:

		QUADRUPLE
		+------------------+
		| codeword         |
		+------------------+		   DOUBLE
		| addr of DOUBLE  ---------------> +------------------+
		+------------------+               | codeword         |
	%esi ->	| addr of DOUBLE   |		   +------------------+
		+------------------+	   	   | addr of DUP      |
		| addr of EXIT	   |		   +------------------+
		+------------------+	   	   | addr of +        |
						   +------------------+
						   | addr of EXIT     |
						   +------------------+

	And NEXT just completes the job by, well in this case just by calling DOUBLE again :-)

	LITERALS ----------------------------------------------------------------------

	The final point I "glossed over" before was how to deal with functions that do anything
	apart from calling other functions.  For example, suppose that DOUBLE was defined like this:

	: DOUBLE 2 * ;

	It does the same thing, but how do we compile it since it contains the literal 2?  One way
	would be to have a function called "2" (which you'd have to write in assembler), but you'd need
	a function for every single literal that you wanted to use.

	FORTH solves this by compiling the function using a special word called LIT:

	+---------------------------+-------+-------+-------+-------+-------+
	| (usual header of DOUBLE)  | DOCOL | LIT   | 2     | *     | EXIT  |
	+---------------------------+-------+-------+-------+-------+-------+

	LIT is executed in the normal way, but what it does next is definitely not normal.  It
	looks at %esi (which now points to the literal 2), grabs it, pushes it on the stack, then
	manipulates %esi in order to skip the literal as if it had never been there.

	What's neat is that the whole grab/manipulate can be done using a single byte single
	i386 instruction, our old friend LODSL.  Rather than me drawing more ASCII-art diagrams,
	see if you can find out how LIT works:
*/

	defcode "LIT",3,,LIT
	// %esi points to the next command, but in this case it points to the next
	// literal 32 bit integer.  Get that literal into %eax and increment %esi.
	// On x86, it's a convenient single byte instruction!  (cf. NEXT macro)
	lodsl
	push %eax		// push the literal number on to stack
	NEXT

/*
	MEMORY ----------------------------------------------------------------------

	As important point about FORTH is that it gives you direct access to the lowest levels
	of the machine.  Manipulating memory directly is done frequently in FORTH, and these are
	the primitive words for doing it.
*/

	defcode "!",1,,STORE
	pop %ebx		// address to store at
	pop %eax		// data to store there
	mov %eax,(%ebx)		// store it
	NEXT

	defcode "@",1,,FETCH
	pop %ebx		// address to fetch
	mov (%ebx),%eax		// fetch it
	push %eax		// push value onto stack
	NEXT

	defcode "+!",2,,ADDSTORE
	pop %ebx		// address
	pop %eax		// the amount to add
	addl %eax,(%ebx)	// add it
	NEXT

	defcode "-!",2,,SUBSTORE
	pop %ebx		// address
	pop %eax		// the amount to subtract
	subl %eax,(%ebx)	// add it
	NEXT

/* ! and @ (STORE and FETCH) store 32-bit words.  It's also useful to be able to read and write bytes.
 * I don't know whether FORTH has these words, so I invented my own, called !b and @b.
 * Byte-oriented operations only work on architectures which permit them (i386 is one of those).
 * UPDATE: writing a byte to the dictionary pointer is called C, in FORTH.
 */
	defcode "!b",2,,STOREBYTE
	pop %ebx		// address to store at
	pop %eax		// data to store there
	movb %al,(%ebx)		// store it
	NEXT

	defcode "@b",2,,FETCHBYTE
	pop %ebx		// address to fetch
	xor %eax,%eax
	movb (%ebx),%al		// fetch it
	push %eax		// push value onto stack
	NEXT

/*
	BUILT-IN VARIABLES ----------------------------------------------------------------------

	These are some built-in variables and related standard FORTH words.  Of these, the only one that we
	have discussed so far was LATEST, which points to the last (most recently defined) word in the
	FORTH dictionary.  LATEST is also a FORTH word which pushes the address of LATEST (the variable)
	on to the stack, so you can read or write it using @ and ! operators.  For example, to print
	the current value of LATEST (and this can apply to any FORTH variable) you would do:

	LATEST @ . CR

	To make defining variables shorter, I'm using a macro called defvar, similar to defword and
	defcode above.  (In fact the defvar macro uses defcode to do the dictionary header).
*/

	.macro defvar name, namelen, flags=0, label, initial=0
	defcode \name,\namelen,\flags,\label
	push $var_\name
	NEXT
	.data
	.align 4
var_\name :
	.int \initial
	.endm

/*
	The built-in variables are:

	STATE		Is the interpreter executing code (0) or compiling a word (non-zero)?
	LATEST		Points to the latest (most recently defined) word in the dictionary.
	HERE		Points to the next free byte of memory.  When compiling, compiled words go here.
	_X		These are three scratch variables, used by some standard dictionary words.
	_Y
	_Z
	S0		Stores the address of the top of the parameter stack.
	BASE		The current base for printing and reading numbers.

*/
	defvar "STATE",5,,STATE
	defvar "HERE",4,,HERE,user_defs_start
	defvar "LATEST",6,,LATEST,name_SYSEXIT // SYSEXIT must be last in built-in dictionary
	defvar "_X",2,,TX
	defvar "_Y",2,,TY
	defvar "_Z",2,,TZ
	defvar "S0",2,,SZ
	defvar "BASE",4,,BASE,10

/*
	BUILT-IN CONSTANTS ----------------------------------------------------------------------

	It's also useful to expose a few constants to FORTH.  When the word is executed it pushes a
	constant value on the stack.

	The built-in constants are:

	VERSION		Is the current version of this FORTH.
	R0		The address of the top of the return stack.
	DOCOL		Pointer to DOCOL.
	F_IMMED		The IMMEDIATE flag's actual value.
	F_HIDDEN	The HIDDEN flag's actual value.
	F_LENMASK	The length mask.
*/

	.macro defconst name, namelen, flags=0, label, value
	defcode \name,\namelen,\flags,\label
	push $\value
	NEXT
	.endm

	defconst "VERSION",7,,VERSION,JONES_VERSION
	defconst "R0",2,,RZ,return_stack
	defconst "DOCOL",5,,__DOCOL,DOCOL
	defconst "F_IMMED",7,,__F_IMMED,F_IMMED
	defconst "F_HIDDEN",8,,__F_HIDDEN,F_HIDDEN
	defconst "F_LENMASK",9,,__F_LENMASK,F_LENMASK

/*
	RETURN STACK ----------------------------------------------------------------------

	These words allow you to access the return stack.  Recall that the register %ebp always points to
	the top of the return stack.
*/

	defcode ">R",2,,TOR
	pop %eax		// pop parameter stack into %eax
	PUSHRSP %eax		// push it on to the return stack
	NEXT

	defcode "R>",2,,FROMR
	POPRSP %eax		// pop return stack on to %eax
	push %eax		// and push on to parameter stack
	NEXT

	defcode "RSP@",4,,RSPFETCH
	push %ebp
	NEXT

	defcode "RSP!",4,,RSPSTORE
	pop %ebp
	NEXT

	defcode "RDROP",5,,RDROP
	lea 4(%ebp),%ebp	// pop return stack and throw away
	NEXT

/*
	PARAMETER (DATA) STACK ----------------------------------------------------------------------

	These functions allow you to manipulate the parameter stack.  Recall that Linux sets up the parameter
	stack for us, and it is accessed through %esp.
*/

	defcode "DSP@",4,,DSPFETCH
	mov %esp,%eax
	push %eax
	NEXT

	defcode "DSP!",4,,DSPSTORE
	pop %esp
	NEXT

/*
	INPUT AND OUTPUT ----------------------------------------------------------------------

	These are our first really meaty/complicated FORTH primitives.  I have chosen to write them in
	assembler, but surprisingly in "real" FORTH implementations these are often written in terms
	of more fundamental FORTH primitives.  I chose to avoid that because I think that just obscures
	the implementation.  After all, you may not understand assembler but you can just think of it
	as an opaque block of code that does what it says.

	Let's discuss input first.

	The FORTH word KEY reads the next byte from stdin (and pushes it on the parameter stack).
	So if KEY is called and someone hits the space key, then the number 32 (ASCII code of space)
	is pushed on the stack.

	In FORTH there is no distinction between reading code and reading input.  We might be reading
	and compiling code, we might be reading words to execute, we might be asking for the user
	to type their name -- ultimately it all comes in through KEY.

	The implementation of KEY uses an input buffer of a certain size (defined at the end of the
	program).  It calls the Linux read(2) system call to fill this buffer and tracks its position
	in the buffer using a couple of variables, and if it runs out of input buffer then it refills
	it automatically.  The other thing that KEY does is if it detects that stdin has closed, it
	exits the program, which is why when you hit ^D the FORTH system cleanly exits.
*/

#include <asm-i386/unistd.h>

	defcode "KEY",3,,KEY
	call _KEY
	push %eax		// push return value on stack
	NEXT
_KEY:
	mov (currkey),%ebx
	cmp (bufftop),%ebx
	jge 1f
	xor %eax,%eax
	mov (%ebx),%al
	inc %ebx
	mov %ebx,(currkey)
	ret

1:	// out of input; use read(2) to fetch more input from stdin
	xor %ebx,%ebx		// 1st param: stdin
	mov $buffer,%ecx	// 2nd param: buffer
	mov %ecx,currkey
	mov $buffend-buffer,%edx // 3rd param: max length
	mov $__NR_read,%eax	// syscall: read
	int $0x80
	test %eax,%eax		// If %eax <= 0, then exit.
	jbe 2f
	addl %eax,%ecx		// buffer+%eax = bufftop
	mov %ecx,bufftop
	jmp _KEY

2:	// error or out of input: exit
	xor %ebx,%ebx
	mov $__NR_exit,%eax	// syscall: exit
	int $0x80

/*
	By contrast, output is much simpler.  The FORTH word EMIT writes out a single byte to stdout.
	This implementation just uses the write system call.  No attempt is made to buffer output, but
	it would be a good exercise to add it.
*/

	defcode "EMIT",4,,EMIT
	pop %eax
	call _EMIT
	NEXT
_EMIT:
	mov $1,%ebx		// 1st param: stdout

	// write needs the address of the byte to write
	mov %al,(2f)
	mov $2f,%ecx		// 2nd param: address

	mov $1,%edx		// 3rd param: nbytes = 1

	mov $__NR_write,%eax	// write syscall
	int $0x80
	ret

	.bss
2:	.space 1		// scratch used by EMIT

/*
	Back to input, WORD is a FORTH word which reads the next full word of input.

	What it does in detail is that it first skips any blanks (spaces, tabs, newlines and so on).
	Then it calls KEY to read characters into an internal buffer until it hits a blank.  Then it
	calculates the length of the word it read and returns the address and the length as
	two words on the stack (with address at the top).

	Notice that WORD has a single internal buffer which it overwrites each time (rather like
	a static C string).  Also notice that WORD's internal buffer is just 32 bytes long and
	there is NO checking for overflow.  31 bytes happens to be the maximum length of a
	FORTH word that we support, and that is what WORD is used for: to read FORTH words when
	we are compiling and executing code.  The returned strings are not NUL-terminated, so
	in some crazy-world you could define FORTH words containing ASCII NULs, although why
	you'd want to is a bit beyond me.

	WORD is not suitable for just reading strings (eg. user input) because of all the above
	peculiarities and limitations.

	Note that when executing, you'll see:
	WORD FOO
	which puts "FOO" and length 3 on the stack, but when compiling:
	: BAR WORD FOO ;
	is an error (or at least it doesn't do what you might expect).  Later we'll talk about compiling
	and immediate mode, and you'll understand why.
*/

	defcode "WORD",4,,WORD
	call _WORD
	push %ecx		// push length
	push %edi		// push base address
	NEXT

_WORD:
	/* Search for first non-blank character.  Also skip \ comments. */
1:
	call _KEY		// get next key, returned in %eax
	cmpb $'\\',%al		// start of a comment?
	je 3f			// if so, skip the comment
	cmpb $' ',%al
	jbe 1b			// if so, keep looking

	/* Search for the end of the word, storing chars as we go. */
	mov $5f,%edi		// pointer to return buffer
2:
	stosb			// add character to return buffer
	call _KEY		// get next key, returned in %al
	cmpb $' ',%al		// is blank?
	ja 2b			// if not, keep looping

	/* Return the word (well, the static buffer) and length. */
	sub $5f,%edi
	mov %edi,%ecx		// return length of the word
	mov $5f,%edi		// return address of the word
	ret

	/* Code to skip \ comments to end of the current line. */
3:
	call _KEY
	cmpb $'\n',%al		// end of line yet?
	jne 3b
	jmp 1b

	.bss
	// A static buffer where WORD returns.  Subsequent calls
	// overwrite this buffer.  Maximum word length is 32 chars.
5:	.space 32

/*
	. (also called DOT) prints the top of the stack as an integer in the current BASE.
*/

	defcode ".",1,,DOT
	pop %eax		// Get the number to print into %eax
	call _DOT		// Easier to do this recursively ...
	NEXT
_DOT:
	mov var_BASE,%ecx	// Get current BASE
1:
	cmp %ecx,%eax		// %eax < BASE?  If so jump to print immediately.
	jb 2f
	xor %edx,%edx		// %edx:%eax / %ecx -> quotient %eax, remainder %edx
	idivl %ecx
	pushl %edx		// Print quotient (top half) first ...
	call _DOT
	popl %eax		// ... then loop to print remainder
	jmp 1b
2:				// %eax < BASE so print immediately.
	movl $digits,%edx
	addl %eax,%edx
	movb (%edx),%al		// Note top bits are already zero.
	call _EMIT
	ret
	.section .rodata
digits:	.ascii "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

/*
	Almost the opposite of DOT (but not quite), SNUMBER parses a numeric string such as one returned
	by WORD and pushes the number on the parameter stack.

	This function does absolutely no error checking, and in particular the length of the string
	must be >= 1 bytes, and should contain only digits 0-9.  If it doesn't you'll get random results.

	This function is only used when reading literal numbers in code, and shouldn't really be used
	in user code at all.
*/
	defcode "SNUMBER",7,,SNUMBER
	pop %edi
	pop %ecx
	call _SNUMBER
	push %eax
	NEXT
_SNUMBER:
	xor %eax,%eax
	xor %ebx,%ebx
1:
	imull $10,%eax		// %eax *= 10
	movb (%edi),%bl
	inc %edi
	subb $'0',%bl		// ASCII -> digit
	add %ebx,%eax
	dec %ecx
	jnz 1b
	ret

/*
	DICTIONARY LOOK UPS ----------------------------------------------------------------------

	We're building up to our prelude on how FORTH code is compiled, but first we need yet more infrastructure.

	The FORTH word FIND takes a string (a word as parsed by WORD -- see above) and looks it up in the
	dictionary.  What it actually returns is the address of the dictionary header, if it finds it,
	or 0 if it didn't.

	So if DOUBLE is defined in the dictionary, then WORD DOUBLE FIND returns the following pointer:

    pointer to this
	|
	|
	V
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+

	See also >CFA and >DFA.

	FIND doesn't find dictionary entries which are flagged as HIDDEN.  See below for why.
*/

	defcode "FIND",4,,FIND
	pop %edi		// %edi = address
	pop %ecx		// %ecx = length
	call _FIND
	push %eax
	NEXT

_FIND:
	push %esi		// Save %esi so we can use it in string comparison.

	// Now we start searching backwards through the dictionary for this word.
	mov var_LATEST,%edx	// LATEST points to name header of the latest word in the dictionary
1:
	test %edx,%edx		// NULL pointer?  (end of the linked list)
	je 4f

	// Compare the length expected and the length of the word.
	// Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
	// this won't pick the word (the length will appear to be wrong).
	xor %eax,%eax
	movb 4(%edx),%al	// %al = flags+length field
	andb $(F_HIDDEN|F_LENMASK),%al // %al = name length
	cmpb %cl,%al		// Length is the same?
	jne 2f

	// Compare the strings in detail.
	push %ecx		// Save the length
	push %edi		// Save the address (repe cmpsb will move this pointer)
	lea 5(%edx),%esi	// Dictionary string we are checking against.
	repe cmpsb		// Compare the strings.
	pop %edi
	pop %ecx
	jne 2f			// Not the same.

	// The strings are the same - return the header pointer in %eax
	pop %esi
	mov %edx,%eax
	ret

2:
	mov (%edx),%edx		// Move back through the link field to the previous word
	jmp 1b			// .. and loop.

4:	// Not found.
	pop %esi
	xor %eax,%eax		// Return zero to indicate not found.
	ret

/*
	FIND returns the dictionary pointer, but when compiling we need the codeword pointer (recall
	that FORTH definitions are compiled into lists of codeword pointers).  The standard FORTH
	word >CFA turns a dictionary pointer into a codeword pointer.

	The example below shows the result of:

		WORD DOUBLE FIND >CFA

	FIND returns a pointer to this
	|				>CFA converts it to a pointer to this
	|					   |
	V					   V
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+

	Notes:

	Because names vary in length, this isn't just a simple increment.

	In this FORTH you cannot easily turn a codeword pointer back into a dictionary entry pointer, but
	that is not true in most FORTH implementations where they store a back pointer in the definition
	(with an obvious memory/complexity cost).  The reason they do this is that it is useful to be
	able to go backwards (codeword -> dictionary entry) in order to decompile FORTH definitions.

	What does CFA stand for?  My best guess is "Code Field Address".
*/

	defcode ">CFA",4,,TCFA
	pop %edi
	call _TCFA
	push %edi
	NEXT
_TCFA:
	xor %eax,%eax
	add $4,%edi		// Skip link pointer.
	movb (%edi),%al		// Load flags+len into %al.
	inc %edi		// Skip flags+len byte.
	andb $F_LENMASK,%al	// Just the length, not the flags.
	add %eax,%edi		// Skip the name.
	addl $3,%edi		// The codeword is 4-byte aligned.
	andl $~3,%edi
	ret

/*
	Related to >CFA is >DFA which takes a dictionary entry address as returned by FIND and
	returns a pointer to the first data field.

	FIND returns a pointer to this
	|				>CFA converts it to a pointer to this
	|					   |
	|					   |	>DFA converts it to a pointer to this
	|					   |		 |
	V					   V		 V
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+

	(Note to those following the source of FIG-FORTH / ciforth: My >DFA definition is
	different from theirs, because they have an extra indirection).

	You can see that >DFA is easily defined in FORTH just by adding 4 to the result of >CFA.
*/

	defword ">DFA",4,,TDFA
	.int TCFA		// >CFA		(get code field address)
	.int INCR4		// 4+		(add 4 to it to get to next word)
	.int EXIT		// EXIT		(return from FORTH word)

/*
	COMPILING ----------------------------------------------------------------------

	Now we'll talk about how FORTH compiles words.  Recall that a word definition looks like this:

		: DOUBLE DUP + ;

	and we have to turn this into:

	  pointer to previous word
	   ^
	   |
	+--|------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
	+---------+---+---+---+---+---+---+---+---+------------+--|---------+------------+------------+
           ^       len                         pad  codeword      |
	   |							  V
	  LATEST points here				points to codeword of DUP

	There are several problems to solve.  Where to put the new word?  How do we read words?  How
	do we define the words : (COLON) and ; (SEMICOLON)?

	FORTH solves this rather elegantly and as you might expect in a very low-level way which
	allows you to change how the compiler works on your own code.

	FORTH has an INTERPRETER function (a true interpreter this time, not DOCOL) which runs in a
	loop, reading words (using WORD), looking them up (using FIND), turning them into codeword
	pointers (using >CFA) and deciding what to do with them.

	What it does depends on the mode of the interpreter (in variable STATE).

	When STATE is zero, the interpreter just runs each word as it looks them up.  This is known as
	immediate mode.

	The interesting stuff happens when STATE is non-zero -- compiling mode.  In this mode the
	interpreter appends the codeword pointer to user memory (the HERE variable points to the next
	free byte of user memory).

	So you may be able to see how we could define : (COLON).  The general plan is:

	(1) Use WORD to read the name of the function being defined.

	(2) Construct the dictionary entry -- just the header part -- in user memory:

    pointer to previous word (from LATEST)			+-- Afterwards, HERE points here, where
	   ^							|   the interpreter will start appending
	   |							V   codewords.
	+--|------+---+---+---+---+---+---+---+---+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      |
	+---------+---+---+---+---+---+---+---+---+------------+
                   len                         pad  codeword

	(3) Set LATEST to point to the newly defined word, ...

	(4) .. and most importantly leave HERE pointing just after the new codeword.  This is where
	    the interpreter will append codewords.

	(5) Set STATE to 1.  This goes into compile mode so the interpreter starts appending codewords to
	    our partially-formed header.

	After : has run, our input is here:

	: DOUBLE DUP + ;
	         ^
		 |
		Next byte returned by KEY will be the 'D' character of DUP

	so the interpreter (now it's in compile mode, so I guess it's really the compiler) reads "DUP",
	looks it up in the dictionary, gets its codeword pointer, and appends it:

									     +-- HERE updated to point here.
									     |
									     V
	+---------+---+---+---+---+---+---+---+---+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        |
	+---------+---+---+---+---+---+---+---+---+------------+------------+
                   len                         pad  codeword

	Next we read +, get the codeword pointer, and append it:

											  +-- HERE updated to point here.
											  |
											  V
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          |
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+
                   len                         pad  codeword

	The issue is what happens next.  Obviously what we _don't_ want to happen is that we
	read ";" and compile it and go on compiling everything afterwards.

	At this point, FORTH uses a trick.  Remember the length byte in the dictionary definition
	isn't just a plain length byte, but can also contain flags.  One flag is called the
	IMMEDIATE flag (F_IMMED in this code).  If a word in the dictionary is flagged as
	IMMEDIATE then the interpreter runs it immediately _even if it's in compile mode_.

	This is how the word ; (SEMICOLON) works -- as a word flagged in the dictionary as IMMEDIATE.
	And all it does is append the codeword for EXIT on to the current definition and switch
	back to immediate mode (set STATE back to 0).  Shortly we'll see the actual definition
	of ; and we'll see that it's really a very simple definition, declared IMMEDIATE.

	After the interpreter reads ; and executes it 'immediately', we get this:

	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
                   len                         pad  codeword					       ^
												       |
												      HERE

	STATE is set to 0.

	And that's it, job done, our new definition is compiled, and we're back in immediate mode
	just reading and executing words, perhaps including a call to test our new word DOUBLE.

	The only last wrinkle in this is that while our word was being compiled, it was in a
	half-finished state.  We certainly wouldn't want DOUBLE to be called somehow during
	this time.  There are several ways to stop this from happening, but in FORTH what we
	do is flag the word with the HIDDEN flag (F_HIDDEN in this code) just while it is
	being compiled.  This prevents FIND from finding it, and thus in theory stops any
	chance of it being called.

	The above explains how compiling, : (COLON) and ; (SEMICOLON) works and in a moment I'm
	going to define them.  The : (COLON) function can be made a little bit more general by writing
	it in two parts.  The first part, called CREATE, makes just the header:

						   +-- Afterwards, HERE points here.
						   |
						   V
	+---------+---+---+---+---+---+---+---+---+
	| LINK    | 6 | D | O | U | B | L | E | 0 |
	+---------+---+---+---+---+---+---+---+---+
                   len                         pad

	and the second part, the actual definition of : (COLON), calls CREATE and appends the
	DOCOL codeword, so leaving:

								+-- Afterwards, HERE points here.
								|
								V
	+---------+---+---+---+---+---+---+---+---+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      |
	+---------+---+---+---+---+---+---+---+---+------------+
                   len                         pad  codeword

	CREATE is a standard FORTH word and the advantage of this split is that we can reuse it to
	create other types of words (not just ones which contain code, but words which contain variables,
	constants and other data).
*/

	defcode "CREATE",6,,CREATE

	// Get the word.
	call _WORD		// Returns %ecx = length, %edi = pointer to word.
	mov %edi,%ebx		// %ebx = address of the word

	// Link pointer.
	movl var_HERE,%edi	// %edi is the address of the header
	movl var_LATEST,%eax	// Get link pointer
	stosl			// and store it in the header.

	// Length byte and the word itself.
	mov %cl,%al		// Get the length.
	stosb			// Store the length/flags byte.
	push %esi
	mov %ebx,%esi		// %esi = word
	rep movsb		// Copy the word
	pop %esi
	addl $3,%edi		// Align to next 4 byte boundary.
	andl $~3,%edi

	// Update LATEST and HERE.
	movl var_HERE,%eax
	movl %eax,var_LATEST
	movl %edi,var_HERE
	NEXT

/*
	Because I want to define : (COLON) in FORTH, not assembler, we need a few more FORTH words
	to use.

	The first is , (COMMA) which is a standard FORTH word which appends a 32 bit integer to the user
	data area pointed to by HERE, and adds 4 to HERE.  So the action of , (COMMA) is:

							previous value of HERE
								 |
								 V
	+---------+---+---+---+---+---+---+---+---+-- - - - - --+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 |             |  <data>    |
	+---------+---+---+---+---+---+---+---+---+-- - - - - --+------------+
                   len                         pad		              ^
									      |
									new value of HERE

	and <data> is whatever 32 bit integer was at the top of the stack.

	, (COMMA) is quite a fundamental operation when compiling.  It is used to append codewords
	to the current word that is being compiled.
*/

	defcode ",",1,,COMMA
	pop %eax		// Code pointer to store.
	call _COMMA
	NEXT
_COMMA:
	movl var_HERE,%edi	// HERE
	stosl			// Store it.
	movl %edi,var_HERE	// Update HERE (incremented)
	ret

/*
	Our definitions of : (COLON) and ; (SEMICOLON) will need to switch to and from compile mode.

	Immediate mode vs. compile mode is stored in the global variable STATE, and by updating this
	variable we can switch between the two modes.

	For various reasons which may become apparent later, FORTH defines two standard words called
	[ and ] (LBRAC and RBRAC) which switch between modes:

	Word	Assembler	Action		Effect
	[	LBRAC		STATE := 0	Switch to immediate mode.
	]	RBRAC		STATE := 1	Switch to compile mode.

	[ (LBRAC) is an IMMEDIATE word.  The reason is as follows: If we are in compile mode and the
	interpreter saw [ then it would compile it rather than running it.  We would never be able to
	switch back to immediate mode!  So we flag the word as IMMEDIATE so that even in compile mode
	the word runs immediately, switching us back to immediate mode.
*/

	defcode "[",1,F_IMMED,LBRAC
	xor %eax,%eax
	movl %eax,var_STATE	// Set STATE to 0.
	NEXT

	defcode "]",1,,RBRAC
	movl $1,var_STATE	// Set STATE to 1.
	NEXT

/*
	Now we can define : (COLON) using CREATE.  It just calls CREATE, appends DOCOL (the codeword), sets
	the word HIDDEN and goes into compile mode.
*/

	defword ":",1,,COLON
	.int CREATE		// CREATE the dictionary entry / header
	.int LIT, DOCOL, COMMA	// Append DOCOL  (the codeword).
	.int LATEST, FETCH, HIDDEN // Make the word hidden (see below for definition).
	.int RBRAC		// Go into compile mode.
	.int EXIT		// Return from the function.

/*
	; (SEMICOLON) is also elegantly simple.  Notice the F_IMMED flag.
*/

	defword ";",1,F_IMMED,SEMICOLON
	.int LIT, EXIT, COMMA	// Append EXIT (so the word will return).
	.int LATEST, FETCH, HIDDEN // Toggle hidden flag -- unhide the word (see below for definition).
	.int LBRAC		// Go back to IMMEDIATE mode.
	.int EXIT		// Return from the function.

/*
	EXTENDING THE COMPILER ----------------------------------------------------------------------

	Words flagged with IMMEDIATE (F_IMMED) aren't just for the FORTH compiler to use.  You can define
	your own IMMEDIATE words too, and this is a crucial aspect when extending basic FORTH, because
	it allows you in effect to extend the compiler itself.  Does gcc let you do that?

	Standard FORTH words like IF, WHILE, ." and so on are all written as extensions to the basic
	compiler, and are all IMMEDIATE words.

	The IMMEDIATE word toggles the F_IMMED (IMMEDIATE flag) on the most recently defined word,
	or on the current word if you call it in the middle of a definition.

	Typical usage is:

	: MYIMMEDWORD IMMEDIATE
		...definition...
	;

	but some FORTH programmers write this instead:

	: MYIMMEDWORD
		...definition...
	; IMMEDIATE

	The two usages are equivalent, to a first approximation.
*/

	defcode "IMMEDIATE",9,F_IMMED,IMMEDIATE
	movl var_LATEST,%edi	// LATEST word.
	addl $4,%edi		// Point to name/flags byte.
	xorb $F_IMMED,(%edi)	// Toggle the IMMED bit.
	NEXT

/*
	'addr HIDDEN' toggles the hidden flag (F_HIDDEN) of the word defined at addr.  To hide the
	most recently defined word (used above in : and ; definitions) you would do:

		LATEST @ HIDDEN

	Setting this flag stops the word from being found by FIND, and so can be used to make 'private'
	words.  For example, to break up a large word into smaller parts you might do:

		: SUB1 ... subword ... ;
		: SUB2 ... subword ... ;
		: SUB3 ... subword ... ;
		: MAIN ... defined in terms of SUB1, SUB2, SUB3 ... ;
		WORD SUB1 FIND HIDDEN		\ Hide SUB1
		WORD SUB2 FIND HIDDEN		\ Hide SUB2
		WORD SUB3 FIND HIDDEN		\ Hide SUB3

	After this, only MAIN is 'exported' or seen by the rest of the program.
*/

	defcode "HIDDEN",6,,HIDDEN
	pop %edi		// Dictionary entry.
	addl $4,%edi		// Point to name/flags byte.
	xorb $F_HIDDEN,(%edi)	// Toggle the HIDDEN bit.
	NEXT

/*
	' (TICK) is a standard FORTH word which returns the codeword pointer of the next word.

	The common usage is:

	' FOO ,

	which appends the codeword of FOO to the current word we are defining (this only works in compiled code).

	You tend to use ' in IMMEDIATE words.  For example an alternate (and rather useless) way to define
	a literal 2 might be:

	: LIT2 IMMEDIATE
		' LIT ,		\ Appends LIT to the currently-being-defined word
		2 ,		\ Appends the number 2 to the currently-being-defined word
	;

	So you could do:

	: DOUBLE LIT2 * ;

	(If you don't understand how LIT2 works, then you should review the material about compiling words
	and immediate mode).

	This definition of ' uses a cheat which I copied from buzzard92.  As a result it only works in
	compiled code.  It is possible to write a version of ' based on WORD, FIND, >CFA which works in
	immediate mode too.
*/
	defcode "'",1,,TICK
	lodsl			// Get the address of the next word and skip it.
	pushl %eax		// Push it on the stack.
	NEXT

/*
	BRANCHING ----------------------------------------------------------------------

	It turns out that all you need in order to define looping constructs, IF-statements, etc.
	are two primitives.

	BRANCH is an unconditional branch. 0BRANCH is a conditional branch (it only branches if the
	top of stack is zero).

	The diagram below shows how BRANCH works in some imaginary compiled word.  When BRANCH executes,
	%esi starts by pointing to the offset field (compare to LIT above):

	+---------------------+-------+---- - - ---+------------+------------+---- - - - ----+------------+
	| (Dictionary header) | DOCOL |            | BRANCH     | offset     | (skipped)     | word       |
	+---------------------+-------+---- - - ---+------------+-----|------+---- - - - ----+------------+
								   ^  |			      ^
								   |  |			      |
								   |  +-----------------------+
								  %esi added to offset

	The offset is added to %esi to make the new %esi, and the result is that when NEXT runs, execution
	continues at the branch target.  Negative offsets work as expected.

	0BRANCH is the same except the branch happens conditionally.

	Now standard FORTH words such as IF, THEN, ELSE, WHILE, REPEAT, etc. can be implemented entirely
	in FORTH.  They are IMMEDIATE words which append various combinations of BRANCH or 0BRANCH
	into the word currently being compiled.

	As an example, code written like this:

		condition-code IF true-part THEN rest-code

	compiles to:

		condition-code 0BRANCH OFFSET true-part rest-code
					  |		^
					  |		|
					  +-------------+
*/

	defcode "BRANCH",6,,BRANCH
	add (%esi),%esi		// add the offset to the instruction pointer
	NEXT

	defcode "0BRANCH",7,,ZBRANCH
	pop %eax
	test %eax,%eax		// top of stack is zero?
	jz code_BRANCH		// if so, jump back to the branch function above
	lodsl			// otherwise we need to skip the offset
	NEXT

/*
	PRINTING STRINGS ----------------------------------------------------------------------

	LITSTRING and EMITSTRING are primitives used to implement the ." and S" operators
	(which are written in FORTH).  See the definition of those operators below.
*/

	defcode "LITSTRING",9,,LITSTRING
	lodsl			// get the length of the string
	push %eax		// push it on the stack
	push %esi		// push the address of the start of the string
	addl %eax,%esi		// skip past the string
 	addl $3,%esi		// but round up to next 4 byte boundary
	andl $~3,%esi
	NEXT

	defcode "EMITSTRING",10,,EMITSTRING
	mov $1,%ebx		// 1st param: stdout
	pop %ecx		// 2nd param: address of string
	pop %edx		// 3rd param: length of string
	mov $__NR_write,%eax	// write syscall
	int $0x80
	NEXT

/*
	COLD START AND INTERPRETER ----------------------------------------------------------------------

	COLD is the first FORTH function called, almost immediately after the FORTH system "boots".

	INTERPRETER is the FORTH interpreter ("toploop", "toplevel" or "REPL" might be a more accurate
	description -- see: http://en.wikipedia.org/wiki/REPL).
*/

	// COLD must not return (ie. must not call EXIT).
	defword "COLD",4,,COLD
	.int INTERPRETER	// call the interpreter loop (never returns)
	.int LIT,1,SYSEXIT	// hmmm, but in case it does, exit(1).

/* This interpreter is pretty simple, but remember that in FORTH you can always override
 * it later with a more powerful one!
 */
	defword "INTERPRETER",11,,INTERPRETER
	.int INTERPRET,RDROP,INTERPRETER

	defcode "INTERPRET",9,,INTERPRET
	call _WORD		// Returns %ecx = length, %edi = pointer to word.

	// Is it in the dictionary?
	xor %eax,%eax
	movl %eax,interpret_is_lit // Not a literal number (not yet anyway ...)
	call _FIND		// Returns %eax = pointer to header or 0 if not found.
	test %eax,%eax		// Found?
	jz 1f

	// In the dictionary.  Is it an IMMEDIATE codeword?
	mov %eax,%edi		// %edi = dictionary entry
	movb 4(%edi),%al	// Get name+flags.
	push %ax		// Just save it for now.
	call _TCFA		// Convert dictionary entry (in %edi) to codeword pointer.
	pop %ax
	andb $F_IMMED,%al	// Is IMMED flag set?
	mov %edi,%eax
	jnz 4f			// If IMMED, jump straight to executing.

	jmp 2f

1:	// Not in the dictionary (not a word) so assume it's a literal number.
	incl interpret_is_lit
	call _SNUMBER		// Returns the parsed number in %eax
	mov %eax,%ebx
	mov $LIT,%eax		// The word is LIT

2:	// Are we compiling or executing?
	movl var_STATE,%edx
	test %edx,%edx
	jz 4f			// Jump if executing.

	// Compiling - just append the word to the current dictionary definition.
	call _COMMA
	mov interpret_is_lit,%ecx // Was it a literal?
	test %ecx,%ecx
	jz 3f
	mov %ebx,%eax		// Yes, so LIT is followed by a number.
	call _COMMA
3:	NEXT

4:	// Executing - run it!
	mov interpret_is_lit,%ecx // Literal?
	test %ecx,%ecx		// Literal?
	jnz 5f

	// Not a literal, execute it now.  This never returns, but the codeword will
	// eventually call NEXT which will reenter the loop in INTERPRETER.
	jmp *(%eax)

5:	// Executing a literal, which means push it on the stack.
	push %ebx
	NEXT

	.data
	.align 4
interpret_is_lit:
	.int 0			// Flag used to record if reading a literal

/*
	ODDS AND ENDS ----------------------------------------------------------------------

	CHAR puts the ASCII code of the first character of the following word on the stack.  For example
	CHAR A puts 65 on the stack.

	SYSEXIT exits the process using Linux exit syscall.

	In this FORTH, SYSEXIT must be the last word in the built-in (assembler) dictionary because we
	initialise the LATEST variable to point to it.  This means that if you want to extend the assembler
	part, you must put new words before SYSEXIT, or else change how LATEST is initialised.
*/

	defcode "CHAR",4,,CHAR
	call _WORD		// Returns %ecx = length, %edi = pointer to word.
	xor %eax,%eax
	movb (%edi),%al		// Get the first character of the word.
	push %eax		// Push it onto the stack.
	NEXT

	// NB: SYSEXIT must be the last entry in the built-in dictionary.
	defcode SYSEXIT,7,,SYSEXIT
	pop %ebx
	mov $__NR_exit,%eax
	int $0x80

/*
	START OF FORTH CODE ----------------------------------------------------------------------

	We've now reached the stage where the FORTH system is running and self-hosting.  All further
	words can be written as FORTH itself, including words like IF, THEN, .", etc which in most
	languages would be considered rather fundamental.

	As a kind of trick, I prefill the input buffer with the initial FORTH code.  Once this code
	has run (when we get to the "OK" prompt), this input buffer is reused for reading any further
	user input.

	Some notes about the code:

	\ (backslash) is the FORTH way to start a comment which goes up to the next newline.  However
	because this is a C-style string, I have to escape the backslash, which is why they appear as
	\\ comment.

	Similarly, any backslashes in the code are doubled, and " becomes \" (eg. the definition of ."
	is written as : .\" ... ;)

	I use indenting to show structure.  The amount of whitespace has no meaning to FORTH however
	except that you must use at least one whitespace character between words, and words themselves
	cannot contain whitespace.

	FORTH is case-sensitive.  Use capslock!

	Enjoy!
*/

	.data
	.align 4096
buffer:
	// Multi-line constant gives 'Warning: unterminated string; newline inserted' messages which you can ignore.
	.ascii "\
\\ Define some character constants
: '\\n'   10 ;
: 'SPACE' 32 ;

\\ CR prints a carriage return
: CR '\\n' EMIT ;

\\ SPACE prints a space
: SPACE 'SPACE' EMIT ;

\\ DUP, DROP are defined in assembly for speed, but this is how you might define them
\\ in FORTH.  Notice use of the scratch variables _X and _Y.
\\ : DUP _X ! _X @ _X @ ;
\\ : DROP _X ! ;

\\ The built-in . (DOT) function doesn't print a space after the number (unlike the real FORTH word).
\\ However this is very easily fixed by redefining . (DOT).  Any built-in word can be redefined.
: .
	.		\\ this refers back to the previous definition (but see also RECURSE below)
	SPACE
;

\\ The 2... versions of the standard operators work on pairs of stack entries.  They're not used
\\ very commonly so not really worth writing in assembler.  Here is how they are defined in FORTH.
: 2DUP OVER OVER ;
: 2DROP DROP DROP ;

\\ More standard FORTH words.
: 2* 2 * ;
: 2/ 2 / ;

\\ Standard words for manipulating BASE.
: DECIMAL 10 BASE ! ;
: HEX 16 BASE ! ;

\\ Standard words for booleans.
: TRUE 1 ;
: FALSE 0 ;
: NOT 0= ;

\\ LITERAL takes whatever is on the stack and compiles LIT <foo>
: LITERAL IMMEDIATE
	' LIT ,		\\ compile LIT
	,		\\ compile the literal itself (from the stack)
	;

\\ Now we can use [ and ] to insert literals which are calculated at compile time.
\\ Within definitions, use [ ... ] LITERAL anywhere that '...' is a constant expression which you
\\ would rather only compute once (at compile time, rather than calculating it each time your word runs).
: ':'
	[		\\ go into immediate mode temporarily
	CHAR :		\\ push the number 58 (ASCII code of colon) on the stack
	]		\\ go back to compile mode
	LITERAL		\\ compile LIT 58 as the definition of ':' word
;

\\ A few more character constants defined the same way as above.
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '\"' [ CHAR \" ] LITERAL ;

\\ So far we have defined only very simple definitions.  Before we can go further, we really need to
\\ make some control structures, like IF ... THEN and loops.  Luckily we can define arbitrary control
\\ structures directly in FORTH.
\\
\\ Please note that the control structures as I have defined them here will only work inside compiled
\\ words.  If you try to type in expressions using IF, etc. in immediate mode, then they won't work.
\\ Making these work in immediate mode is left as an exercise for the reader.

\\ condition IF true-part THEN rest
\\	-- compiles to: --> condition 0BRANCH OFFSET true-part rest
\\	where OFFSET is the offset of 'rest'
\\ condition IF true-part ELSE false-part THEN
\\ 	-- compiles to: --> condition 0BRANCH OFFSET true-part BRANCH OFFSET2 false-part rest
\\	where OFFSET if the offset of false-part and OFFSET2 is the offset of rest

\\ IF is an IMMEDIATE word which compiles 0BRANCH followed by a dummy offset, and places
\\ the address of the 0BRANCH on the stack.  Later when we see THEN, we pop that address
\\ off the stack, calculate the offset, and back-fill the offset.
: IF IMMEDIATE
	' 0BRANCH ,	\\ compile 0BRANCH
	HERE @		\\ save location of the offset on the stack
	0 ,		\\ compile a dummy offset
;

: THEN IMMEDIATE
	DUP
	HERE @ SWAP -	\\ calculate the offset from the address saved on the stack
	SWAP !		\\ store the offset in the back-filled location
;

: ELSE IMMEDIATE
	' BRANCH ,	\\ definite branch to just over the false-part
	HERE @		\\ save location of the offset on the stack
	0 ,		\\ compile a dummy offset
	SWAP		\\ now back-fill the original (IF) offset
	DUP		\\ same as for THEN word above
	HERE @ SWAP -
	SWAP !
;

\\ BEGIN loop-part condition UNTIL
\\	-- compiles to: --> loop-part condition 0BRANCH OFFSET
\\	where OFFSET points back to the loop-part
\\ This is like do { loop-part } while (condition) in the C language
: BEGIN IMMEDIATE
	HERE @		\\ save location on the stack
;

: UNTIL IMMEDIATE
	' 0BRANCH ,	\\ compile 0BRANCH
	HERE @ -	\\ calculate the offset from the address saved on the stack
	,		\\ compile the offset here
;

\\ BEGIN loop-part AGAIN
\\	-- compiles to: --> loop-part BRANCH OFFSET
\\	where OFFSET points back to the loop-part
\\ In other words, an infinite loop which can only be returned from with EXIT
: AGAIN IMMEDIATE
	' BRANCH ,	\\ compile BRANCH
	HERE @ -	\\ calculate the offset back
	,		\\ compile the offset here
;

\\ BEGIN condition WHILE loop-part REPEAT
\\	-- compiles to: --> condition 0BRANCH OFFSET2 loop-part BRANCH OFFSET
\\	where OFFSET points back to condition (the beginning) and OFFSET2 points to after the whole piece of code
\\ So this is like a while (condition) { loop-part } loop in the C language
: WHILE IMMEDIATE
	' 0BRANCH ,	\\ compile 0BRANCH
	HERE @		\\ save location of the offset2 on the stack
	0 ,		\\ compile a dummy offset2
;

: REPEAT IMMEDIATE
	' BRANCH ,	\\ compile BRANCH
	SWAP		\\ get the original offset (from BEGIN)
	HERE @ - ,	\\ and compile it after BRANCH
	DUP
	HERE @ SWAP -	\\ calculate the offset2
	SWAP !		\\ and back-fill it in the original location
;

\\ FORTH allows ( ... ) as comments within function definitions.  This works by having an IMMEDIATE
\\ word called ( which just drops input characters until it hits the corresponding ).
: ( IMMEDIATE
	1		\\ allowed nested parens by keeping track of depth
	BEGIN
		KEY		\\ read next character
		DUP '(' = IF	\\ open paren?
			DROP		\\ drop the open paren
			1+		\\ depth increases
		ELSE
			')' = IF	\\ close paren?
				1-		\\ depth decreases
			THEN
		THEN
	DUP 0= UNTIL		\\ continue until we reach matching close paren, depth 0
	DROP		\\ drop the depth counter
;

(
	From now on we can use ( ... ) for comments.

	In FORTH style we can also use ( ... -- ... ) to show the effects that a word has on the
	parameter stack.  For example:

	( n -- )	means that the word consumes an integer (n) from the parameter stack.
	( b a -- c )	means that the word uses two integers (a and b, where a is at the top of stack)
				and returns a single integer (c).
	( -- )		means the word has no effect on the stack
)

( With the looping constructs, we can now write SPACES, which writes n spaces to stdout. )
: SPACES	( n -- )
	BEGIN
		DUP 0>		( while n > 0 )
	WHILE
		SPACE		( print a space )
		1-		( until we count down to 0 )
	REPEAT
	DROP
;

( c a b WITHIN returns true if a <= c and c < b )
: WITHIN
	ROT		( b c a )
	OVER		( b c a c )
	<= IF
		> IF		( b c -- )
			TRUE
		ELSE
			FALSE
		THEN
	ELSE
		2DROP		( b c -- )
		FALSE
	THEN
;

( .S prints the contents of the stack.  Very useful for debugging. )
: .S		( -- )
	DSP@		( get current stack pointer )
	BEGIN
		DUP S0 @ <
	WHILE
		DUP @ .		( print the stack element )
		4+		( move up )
	REPEAT
	DROP
;

( DEPTH returns the depth of the stack. )
: DEPTH		( -- n )
	S0 @ DSP@ -
	4-			( adjust because S0 was on the stack when we pushed DSP )
;

(
	[NB. The following may be a bit confusing because of the need to use backslash before
	each double quote character.  The backslashes are there to keep the assembler happy.
	They are NOT part of the final output.  So here we are defining a function called
	'S double-quote' (not 'S backslash double-quote').]

	S\" string\" is used in FORTH to define strings.  It leaves the address of the string and
	its length on the stac,k with the address at the top.  The space following S\" is the normal
	space between FORTH words and is not a part of the string.

	In compile mode we append
		LITSTRING <string length> <string rounded up 4 bytes>
	to the current word.  The primitive LITSTRING does the right thing when the current
	word is executed.

	In immediate mode there isn't a particularly good place to put the string, but in this
	case we put the string at HERE (but we _don't_ change HERE).  This is meant as a temporary
	location, likely to be overwritten soon after.
)
: S\" IMMEDIATE		( -- len addr )
	STATE @ IF	( compiling? )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		BEGIN
			KEY 		( get next character of the string )
			DUP '\"' <>
		WHILE
			HERE @ !b	( store the character in the compiled image )
			1 HERE +!	( increment HERE pointer by 1 byte )
		REPEAT
		DROP		( drop the double quote character at the end )
		DUP		( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		SWAP !		( and back-fill the length location )
		HERE @		( round up to next multiple of 4 bytes for the remaining code )
		3 +
		3 INVERT AND
		HERE !
	ELSE		( immediate mode )
		HERE @		( get the start address of the temporary space )
		BEGIN
			KEY
			DUP '\"' <>
		WHILE
			OVER !b		( save next character )
			1+		( increment address )
		REPEAT
		HERE @ -	( calculate the length )
		HERE @		( push the start address )
	THEN
;

(
	.\" is the print string operator in FORTH.  Example: .\" Something to print\"
	The space after the operator is the ordinary space required between words.

	This is tricky to define because it has to do different things depending on whether
	we are compiling or in immediate mode.  (Thus the word is marked IMMEDIATE so it can
	detect this and do different things).

	In immediate mode we just keep reading characters and printing them until we get to
	the next double quote.

	In compile mode we have the problem of where we're going to store the string (remember
	that the input buffer where the string comes from may be overwritten by the time we
	come round to running the function).  We store the string in the compiled function
	like this:
	..., LITSTRING, string length, string rounded up to 4 bytes, EMITSTRING, ...
)
: .\" IMMEDIATE		( -- )
	STATE @ IF	( compiling? )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		BEGIN
			KEY 		( get next character of the string )
			DUP '\"' <>
		WHILE
			HERE @ !b	( store the character in the compiled image )
			1 HERE +!	( increment HERE pointer by 1 byte )
		REPEAT
		DROP		( drop the double quote character at the end )
		DUP		( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		SWAP !		( and back-fill the length location )
		HERE @		( round up to next multiple of 4 bytes for the remaining code )
		3 +
		3 INVERT AND
		HERE !
		' EMITSTRING ,	( compile the final EMITSTRING )
	ELSE
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote.  Much simpler than the above code! )
		BEGIN
			KEY
			DUP '\"' = IF
				DROP	( drop the double quote character )
				EXIT	( return from this function )
			THEN
			EMIT
		AGAIN
	THEN
;

(
	In FORTH, global constants and variables are defined like this:

	10 CONSTANT TEN		when TEN is executed, it leaves the integer 10 on the stack
	VARIABLE VAR		when VAR is executed, it leaves the address of VAR on the stack

	Constants can be read by not written, eg:

	TEN . CR		prints 10

	You can read a variable (in this example called VAR) by doing:

	VAR @			leaves the value of VAR on the stack
	VAR @ . CR		prints the value of VAR

	and update the variable by doing:

	20 VAR !		sets VAR to 20

	Note that variables are uninitialised (but see VALUE later on which provides initialised
	variables with a slightly simpler syntax).

	How can we define the words CONSTANT and VARIABLE?

	The trick is to define a new word for the variable itself (eg. if the variable was called
	'VAR' then we would define a new word called VAR).  This is easy to do because we exposed
	dictionary entry creation through the CREATE word (part of the definition of : above).
	A call to CREATE TEN leaves the dictionary entry:

				   +--- HERE
				   |
				   V
	+---------+---+---+---+---+
	| LINK    | 3 | T | E | N |
	+---------+---+---+---+---+
                   len

	For CONSTANT we can continue by appending DOCOL (the codeword), then LIT followed by
	the constant itself and then EXIT, forming a little word definition that returns the
	constant:

	+---------+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 3 | T | E | N | DOCOL      | LIT        | 10         | EXIT       |
	+---------+---+---+---+---+------------+------------+------------+------------+
                   len              codeword

	Notice that this word definition is exactly the same as you would have got if you had
	written : TEN 10 ;
)
: CONSTANT
	CREATE		( make the dictionary entry (the name follows CONSTANT) )
	DOCOL ,		( append DOCOL (the codeword field of this word) )
	' LIT ,		( append the codeword LIT )
	,		( append the value on the top of the stack )
	' EXIT ,	( append the codeword EXIT )
;

(
	VARIABLE is a little bit harder because we need somewhere to put the variable.  There is
	nothing particularly special about the 'user definitions area' (the area of memory pointed
	to by HERE where we have previously just stored new word definitions).  We can slice off
	bits of this memory area to store anything we want, so one possible definition of
	VARIABLE might create this:

	   +--------------------------------------------------------------+
	   |								  |
	   V								  |
	+---------+---------+---+---+---+---+------------+------------+---|--------+------------+
	| <var>   | LINK    | 3 | V | A | R | DOCOL      | LIT        | <addr var> | EXIT       |
	+---------+---------+---+---+---+---+------------+------------+------------+------------+
        		     len              codeword

	where <var> is the place to store the variable, and <addr var> points back to it.

	To make this more general let's define a couple of words which we can use to allocate
	arbitrary memory from the user definitions area.

	First ALLOT, where n ALLOT allocates n bytes of memory.  (Note when calling this that
	it's a very good idea to make sure that n is a multiple of 4, or at least that next time
	a word is compiled that n has been left as a multiple of 4).
)
: ALLOT		( n -- addr )
	HERE @ SWAP	( here n -- )
	HERE +!		( adds n to HERE, after this the old value of HERE is still on the stack )
;

(
	Second, CELLS.  In FORTH the phrase 'n CELLS ALLOT' means allocate n integers of whatever size
	is the natural size for integers on this machine architecture.  On this 32 bit machine therefore
	CELLS just multiplies the top of stack by 4.
)
: CELLS ( n -- n ) 4 * ;

(
	So now we can define VARIABLE easily in much the same way as CONSTANT above.  Refer to the
	diagram above to see what the word that this creates will look like.
)
: VARIABLE
	1 CELLS ALLOT	( allocate 1 cell of memory, push the pointer to this memory )
	CREATE		( make the dictionary entry (the name follows VARIABLE) )
	DOCOL ,		( append DOCOL (the codeword field of this word) )
	' LIT ,		( append the codeword LIT )
	,		( append the pointer to the new memory )
	' EXIT ,	( append the codeword EXIT )
;

(
	VALUEs are like VARIABLEs but with a simpler syntax.  You would generally use them when you
	want a variable which is read often, and written infrequently.

	20 VALUE VAL 	creates VAL with initial value 20
	VAL		pushes the value directly on the stack
	30 TO VAL	updates VAL, setting it to 30

	Notice that 'VAL' on its own doesn't return the address of the value, but the value itself,
	making values simpler and more obvious to use than variables (no indirection through '@').
	The price is a more complicated implementation, although despite the complexity there is no
	particular performance penalty at runtime.

	A naive implementation of 'TO' would be quite slow, involving a dictionary search each time.
	But because this is FORTH we have complete control of the compiler so we can compile TO more
	efficiently, turning:
		TO VAL
	into:
		LIT <addr> !
	and calculating <addr> (the address of the value) at compile time.

	Now this is the clever bit.  We'll compile our value like this:

	+---------+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 3 | V | A | L | DOCOL      | LIT        | <value>    | EXIT       |
	+---------+---+---+---+---+------------+------------+------------+------------+
                   len              codeword

	where <value> is the actual value itself.  Note that when VAL executes, it will push the
	value on the stack, which is what we want.

	But what will TO use for the address <addr>?  Why of course a pointer to that <value>:

		code compiled	- - - - --+------------+------------+------------+-- - - - -
		by TO VAL		  | LIT        | <addr>     | !          |
				- - - - --+------------+-----|------+------------+-- - - - -
							     |
							     V
	+---------+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 3 | V | A | L | DOCOL      | LIT        | <value>    | EXIT       |
	+---------+---+---+---+---+------------+------------+------------+------------+
                   len              codeword

	In other words, this is a kind of self-modifying code.

	(Note to the people who want to modify this FORTH to add inlining: values defined this
	way cannot be inlined).
)
: VALUE		( n -- )
	CREATE		( make the dictionary entry (the name follows VALUE) )
	DOCOL ,		( append DOCOL )
	' LIT ,		( append the codeword LIT )
	,		( append the initial value )
	' EXIT ,	( append the codeword EXIT )
;

: TO IMMEDIATE	( n -- )
	WORD		( get the name of the value )
	FIND		( look it up in the dictionary )
	>DFA		( get a pointer to the first data field (the 'LIT') )
	4+		( increment to point at the value )
	STATE @ IF	( compiling? )
		' LIT ,		( compile LIT )
		,		( compile the address of the value )
		' ! ,		( compile ! )
	ELSE		( immediate mode )
		!		( update it straightaway )
	THEN
;

( x +TO VAL adds x to VAL )
: +TO IMMEDIATE
	WORD		( get the name of the value )
	FIND		( look it up in the dictionary )
	>DFA		( get a pointer to the first data field (the 'LIT') )
	4+		( increment to point at the value )
	STATE @ IF	( compiling? )
		' LIT ,		( compile LIT )
		,		( compile the address of the value )
		' +! ,		( compile +! )
	ELSE		( immediate mode )
		+!		( update it straightaway )
	THEN
;

(
	ID. takes an address of a dictionary entry and prints the word's name.

	For example: LATEST @ ID. would print the name of the last word that was defined.
)
: ID.
	4+		( skip over the link pointer )
	DUP @b		( get the flags/length byte )
	F_LENMASK AND	( mask out the flags - just want the length )

	BEGIN
		DUP 0>		( length > 0? )
	WHILE
		SWAP 1+		( addr len -- len addr+1 )
		DUP @b		( len addr -- len addr char | get the next character)
		EMIT		( len addr char -- len addr | and print it)
		SWAP 1-		( len addr -- addr len-1    | subtract one from length )
	REPEAT
	2DROP		( len addr -- )
;

(
	'WORD word FIND ?HIDDEN' returns true if 'word' is flagged as hidden.

	'WORD word FIND ?IMMEDIATE' returns true if 'word' is flagged as immediate.
)
: ?HIDDEN
	4+		( skip over the link pointer )
	@b		( get the flags/length byte )
	F_HIDDEN AND	( mask the F_HIDDEN flag and return it (as a truth value) )
;
: ?IMMEDIATE
	4+		( skip over the link pointer )
	@b		( get the flags/length byte )
	F_IMMED AND	( mask the F_IMMED flag and return it (as a truth value) )
;

(
	WORDS prints all the words defined in the dictionary, starting with the word defined most recently.
	However it doesn't print hidden words.

	The implementation simply iterates backwards from LATEST using the link pointers.
)
: WORDS
	LATEST @	( start at LATEST dictionary entry )
	BEGIN
		DUP 0<>		( while link pointer is not null )
	WHILE
		DUP ?HIDDEN NOT IF
			DUP ID.		( print the word )
		THEN
		SPACE
		@		( dereference the link pointer - go to previous word )
	REPEAT
	DROP
	CR
;

(
	So far we have only allocated words and memory.  FORTH provides a rather primitive method
	to deallocate.

	'FORGET word' deletes the definition of 'word' from the dictionary and everything defined
	after it, including any variables and other memory allocated after.

	The implementation is very simple - we look up the word (which returns the dictionary entry
	address).  Then we set HERE to point to that address, so in effect all future allocations
	and definitions will overwrite memory starting at the word.  We also need to set LATEST to
	point to the previous word.

	Note that you cannot FORGET built-in words (well, you can try but it will probably cause
	a segfault).

	XXX: Because we wrote VARIABLE to store the variable in memory allocated before the word,
	in the current implementation VARIABLE FOO FORGET FOO will leak 1 cell of memory.
)
: FORGET
	WORD FIND	( find the word, gets the dictionary entry address )
	DUP @ LATEST !	( set LATEST to point to the previous word )
	HERE !		( and store HERE with the dictionary address )
;

(
	While compiling, '[COMPILE] word' compiles 'word' if it would otherwise be IMMEDIATE.
)
: [COMPILE] IMMEDIATE
	WORD		( get the next word )
	FIND		( find it in the dictionary )
	>CFA		( get its codeword )
	,		( and compile that )
;

(
	RECURSE makes a recursive call to the current word that is being compiled.

	Normally while a word is being compiled, it is marked HIDDEN so that references to the
	same word within are calls to the previous definition of the word.  However we still have
	access to the word which we are currently compiling through the LATEST pointer so we
	can use that to compile a recursive call.
)
: RECURSE IMMEDIATE
	LATEST @ >CFA	( LATEST points to the word being compiled at the moment )
	,		( compile it )
;

(
	DUMP is used to dump out the contents of memory, in the 'traditional' hexdump format.
)
: DUMP		( addr len -- )
	BASE @ ROT		( save the current BASE at the bottom of the stack )
	HEX			( and switch the hexadecimal mode )

	BEGIN
		DUP 0>		( while len > 0 )
	WHILE
		OVER .		( print the address )
		SPACE

		( print up to 16 words on this line )
		2DUP		( addr len addr len )
		1- 15 AND 1+	( addr len addr linelen )
		BEGIN
			DUP 0>		( while linelen > 0 )
		WHILE
			SWAP		( addr len linelen addr )
			DUP @b		( addr len linelen addr byte )
			. SPACE		( print the byte )
			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		REPEAT
		2DROP		( addr len )

		( print the ASCII equivalents )
		2DUP 1- 15 AND 1+ ( addr len addr linelen )
		BEGIN
			DUP 0>		( while linelen > 0)
		WHILE
			SWAP		( addr len linelen addr )
			DUP @b		( addr len linelen addr byte )
			DUP 32 128 WITHIN IF	( 32 <= c < 128? )
				EMIT
			ELSE
				DROP [ CHAR ? ] LITERAL EMIT
			THEN
			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		REPEAT
		2DROP		( addr len )
		CR

		DUP 1- 15 AND 1+ ( addr len linelen )
		DUP		( addr len linelen linelen )
		ROT		( addr linelen len linelen )
		-		( addr linelen len-linelen )
		ROT		( len-linelen addr linelen )
		+		( len-linelen addr+linelen )
		SWAP		( addr-linelen len-linelen )
	REPEAT

	2DROP			( restore stack )
	BASE !			( restore saved BASE )
;

( Finally print the welcome prompt. )
.\" JONESFORTH VERSION \" VERSION . CR
.\" OK \"
"

_initbufftop:
	.align 4096
buffend:

currkey:
	.int buffer
bufftop:
	.int _initbufftop

/* END OF jonesforth.S */
