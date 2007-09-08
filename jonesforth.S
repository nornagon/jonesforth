/*	A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems. -*- asm -*-
	By Richard W.M. Jones <rich@annexia.org> http://annexia.org/forth

	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o jonesforth jonesforth.S

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

	Here is another "Why FORTH?" essay: http://www.jwdt.com/~paysan/why-forth.html

	ACKNOWLEDGEMENTS ----------------------------------------------------------------------

	This code draws heavily on the design of LINA FORTH (http://home.hccnet.nl/a.w.m.van.der.horst/lina.html)
	by Albert van der Horst.  Any similarities in the code are probably not accidental.

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

	In FORTH as you will know, functions are called "words", as just as in other languages they
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

	You shoud be able to see from this how you might implement functions to find a word in
	the dictionary (just walk along the dictionary entries starting at LATEST and matching
	the names until you either find a match or hit the NULL pointer at the end of the dictionary),
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
	16 bit machine code.

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

	The all-important x86 instruction is called LODSL (or in Intel manuals, LODSW).  It does
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
									now we're  | assembly to   <------+
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
		+------------------+	   	   | addr of DUP   -------------->
		| addr of EXIT	   |		   +------------------+
		+------------------+               | etc.             |

	First, the call to DOUBLE causes DOCOL (the codeword of DOUBLE).  DOCOL does this:  It
	pushes the old %esi on the return stack.  %eax points to the codeword of DOUBLE, so we
	just add 4 on to it to get our new %esi:

		QUADRUPLE:
		+------------------+
		| codeword         |
		+------------------+		   DOUBLE:
		| addr of DOUBLE  ---------------> +------------------+
top of return	+------------------+       %eax -> | addr of DOCOL    |
stack points ->	| addr of DOUBLE   |	   + 4 =   +------------------+
		+------------------+	   %esi -> | addr of DUP   -------------->
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
#define RETURN_STACK_SIZE 8192
	.align 4096
	.space RETURN_STACK_SIZE
return_stack:			// Initial top of return stack.

/* Space for user-defined words. */
#define USER_DEFS_SIZE 16384
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
           ^       len              codeword			    which is ended with NEXT.
	   |
	  LINK in next word

	Again, for brevity in writing the header I'm going to use an assembler macro called defcode.
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
	addl $4,(%esp)		// increment top of stack
	NEXT

	defcode "4-",2,,DECR4
	subl $4,(%esp)		// decrement top of stack
	NEXT

	defcode "+",1,,ADD
	pop %eax
	addl %eax,(%esp)
	NEXT

	defcode "-",1,,SUB
	pop %eax
	subl %eax,(%esp)
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

	defcode "0=",2,,ZEQU	// top of stack equals 0?
	pop %eax
	test %eax,%eax
	jz 1f
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

	defcode "INVERT",6,,INVERT
	notl (%esp)
	NEXT

/* Flags. */
#define F_IMMED 0x80
#define F_HIDDEN 0x20

	// COLD must not return (ie. must not call EXIT).
	defword "COLD",4,,COLD
	// XXX reinitialisation of the interpreter
	.int INTERPRETER	// call the interpreter loop (never returns)
	.int LIT,1,SYSEXIT	// hmmm, but in case it does, exit(1).

	defcode "EXIT",4,,EXIT
	POPRSP %esi		// pop return stack into %esi
	NEXT

	defcode "LIT",3,,LIT
	// %esi points to the next command, but in this case it points to the next
	// literal 32 bit integer.  Get that literal into %eax and increment %esi.
	// On x86, it's a convenient single byte instruction!  (cf. NEXT macro)
	lodsl
	push %eax		// push the literal number on to stack
	NEXT

	defcode "LITSTRING",9,,LITSTRING
	lodsl			// get the length of the string
	push %eax		// push it on the stack
	push %esi		// push the address of the start of the string
	addl %eax,%esi		// skip past the string
	addl $3,%esi		// but round up to next 4 byte boundary
	andl $~3,%esi
	NEXT

	defcode "BRANCH",6,,BRANCH
	add (%esi),%esi		// add the offset to the instruction pointer
	NEXT

	defcode "0BRANCH",7,,ZBRANCH
	pop %eax
	test %eax,%eax		// top of stack is zero?
	jz code_BRANCH		// if so, jump back to the branch function above
	lodsl			// otherwise we need to skip the offset
	NEXT

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

	.macro defvar name, namelen, flags=0, label, initial=0
	defcode \name,\namelen,\flags,\label
	push $var_\name
	NEXT
	.data
	.align 4
var_\name :
	.int \initial
	.endm

	// The STATE variable is 0 for execute mode, != 0 for compile mode
	defvar "STATE",5,,STATE

	// This points to where compiled words go.
	defvar "HERE",4,,HERE,user_defs_start

	// This is the last definition in the dictionary.
	defvar "LATEST",6,,LATEST,name_SYSEXIT // SYSEXIT must be last in built-in dictionary

	// _X, _Y and _Z are scratch variables used by standard words.
	defvar "_X",2,,TX
	defvar "_Y",2,,TY
	defvar "_Z",2,,TZ

	// This stores the top of the data stack.
	defvar "S0",2,,SZ

	// This stores the top of the return stack.
	defvar "R0",2,,RZ,return_stack

	defcode "DSP@",4,,DSPFETCH
	mov %esp,%eax
	push %eax
	NEXT

	defcode "DSP!",4,,DSPSTORE
	pop %esp
	NEXT

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

	defcode "EMITSTRING",10,,EMITSTRING
	mov $1,%ebx		// 1st param: stdout
	pop %ecx		// 2nd param: address of string
	pop %edx		// 3rd param: length of string

	mov $__NR_write,%eax	// write syscall
	int $0x80

	NEXT

	defcode ".",1,,DOT
	pop %eax		// Get the number to print into %eax
	call _DOT		// Easier to do this recursively ...
	NEXT
_DOT:
	mov $10,%ecx		// Base 10
1:
	cmp %ecx,%eax
	jb 2f
	xor %edx,%edx		// %edx:%eax / %ecx -> quotient %eax, remainder %edx
	idivl %ecx
	pushl %edx
	call _DOT
	popl %eax
	jmp 1b
2:
	xor %ah,%ah
	aam $10
	cwde
	addl $'0',%eax
	call _EMIT
	ret

	// Parse a number from a string on the stack -- almost the opposite of . (DOT)
	// Note that there is absolutely no error checking.  In particular the length of the
	// string must be >= 1 bytes.
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
	andb $(F_HIDDEN|0x1f),%al // %al = name length
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

	defcode ">CFA",4,,TCFA	// DEA -> Codeword address
	pop %edi
	call _TCFA
	push %edi
	NEXT
_TCFA:
	xor %eax,%eax
	add $4,%edi		// Skip link pointer.
	movb (%edi),%al		// Load flags+len into %al.
	inc %edi		// Skip flags+len byte.
	andb $0x1f,%al		// Just the length, not the flags.
	add %eax,%edi		// Skip the name.
	addl $3,%edi		// The codeword is 4-byte aligned.
	andl $~3,%edi
	ret

	defcode "CHAR",4,,CHAR
	call _WORD		// Returns %ecx = length, %edi = pointer to word.
	xor %eax,%eax
	movb (%edi),%al		// Get the first character of the word.
	push %eax		// Push it onto the stack.
	NEXT

	defcode ":",1,,COLON

	// Get the word and create a dictionary entry header for it.
	call _WORD		// Returns %ecx = length, %edi = pointer to word.
	mov %edi,%ebx		// %ebx = address of the word

	movl var_HERE,%edi	// %edi is the address of the header
	movl var_LATEST,%eax	// Get link pointer
	stosl			// and store it in the header.

	mov %cl,%al		// Get the length.
	orb $F_HIDDEN,%al	// Set the HIDDEN flag on this entry.
	stosb			// Store the length/flags byte.
	push %esi
	mov %ebx,%esi		// %esi = word
	rep movsb		// Copy the word
	pop %esi
	addl $3,%edi		// Align to next 4 byte boundary.
	andl $~3,%edi

	movl $DOCOL,%eax	// The codeword for user-created words is always DOCOL (the interpreter)
	stosl

	// Header built, so now update LATEST and HERE.
	// We'll be compiling words and putting them HERE.
	movl var_HERE,%eax
	movl %eax,var_LATEST
	movl %edi,var_HERE

	// And go into compile mode by setting STATE to 1.
	movl $1,var_STATE
	NEXT

	defcode ",",1,,COMMA
	pop %eax		// Code pointer to store.
	call _COMMA
	NEXT
_COMMA:
	movl var_HERE,%edi	// HERE
	stosl			// Store it.
	movl %edi,var_HERE	// Update HERE (incremented)
	ret

	defcode "HIDDEN",6,,HIDDEN
	call _HIDDEN
	NEXT
_HIDDEN:
	movl var_LATEST,%edi	// LATEST word.
	addl $4,%edi		// Point to name/flags byte.
	xorb $F_HIDDEN,(%edi)	// Toggle the HIDDEN bit.
	ret

	defcode "IMMEDIATE",9,F_IMMED,IMMEDIATE
	call _IMMEDIATE
	NEXT
_IMMEDIATE:
	movl var_LATEST,%edi	// LATEST word.
	addl $4,%edi		// Point to name/flags byte.
	xorb $F_IMMED,(%edi)	// Toggle the IMMED bit.
	ret

	defcode ";",1,F_IMMED,SEMICOLON
	movl $EXIT,%eax		// EXIT is the final codeword in compiled words.
	call _COMMA		// Store it.
	call _HIDDEN		// Toggle the HIDDEN flag (unhides the new word).
	xor %eax,%eax		// Set STATE to 0 (back to execute mode).
	movl %eax,var_STATE
	NEXT

/* This definiton of ' (TICK) is strictly cheating - it also only works in compiled code. */
	defcode "'",1,,TICK
	lodsl			// Get the address of the next word and skip it.
	pushl %eax		// Push it on the stack.
	NEXT

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

	// NB: SYSEXIT must be the last entry in the built-in dictionary.
	defcode SYSEXIT,7,,SYSEXIT
	pop %ebx
	mov $__NR_exit,%eax
	int $0x80

/*----------------------------------------------------------------------
 * Input buffer & initial input.
 */
	.data
	.align 4096
buffer:
	// XXX gives 'Warning: unterminated string; newline inserted' messages which you can ignore
	.ascii "\
\\ Define some character constants
: '\\n'   10 ;
: 'SPACE' 32 ;
: '\"'    34 ;
: ':'     58 ;

\\ CR prints a carriage return
: CR '\\n' EMIT ;

\\ SPACE prints a space
: SPACE 'SPACE' EMIT ;

\\ Primitive . (DOT) function doesn't follow with a blank, so redefine it to behave like FORTH.
\\ Notice how we can trivially redefine existing functions.
: . . SPACE ;

\\ DUP, DROP are defined in assembly for speed, but this is how you might define them
\\ in FORTH.  Notice use of the scratch variables _X and _Y.
\\ : DUP _X ! _X @ _X @ ;
\\ : DROP _X ! ;

\\ The 2... versions of the standard operators work on pairs of stack entries.  They're not used
\\ very commonly so not really worth writing in assembler.  Here is how they are defined in FORTH.
: 2DUP OVER OVER ;
: 2DROP DROP DROP ;

\\ More standard FORTH words.
: 2* 2 * ;
: 2/ 2 / ;

\\ [ and ] allow you to break into immediate mode while compiling a word.
: [ IMMEDIATE		\\ define [ as an immediate word
	0 STATE !	\\ go into immediate mode
	;

: ]
	1 STATE !	\\ go back to compile mode
	;

\\ LITERAL takes whatever is on the stack and compiles LIT <foo>
: LITERAL IMMEDIATE
	' LIT ,		\\ compile LIT
	,		\\ compile the literal itself (from the stack)
	;

\\ condition IF true-part THEN rest
\\   compiles to:
\\ condition 0BRANCH OFFSET true-part rest
\\   where OFFSET is the offset of 'rest'
\\ condition IF true-part ELSE false-part THEN
\\   compiles to:
\\ condition 0BRANCH OFFSET true-part BRANCH OFFSET2 false-part rest
\\   where OFFSET if the offset of false-part and OFFSET2 is the offset of rest

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
\\   compiles to:
\\ loop-part condition 0BRANCH OFFSET
\\   where OFFSET points back to the loop-part
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
\\   compiles to:
\\ loop-part BRANCH OFFSET
\\   where OFFSET points back to the loop-part
\\ In other words, an infinite loop which can only be returned from with EXIT
: AGAIN IMMEDIATE
	' BRANCH ,	\\ compile BRANCH
	HERE @ -	\\ calculate the offset back
	,		\\ compile the offset here
;

\\ BEGIN condition WHILE loop-part REPEAT
\\   compiles to:
\\ condition 0BRANCH OFFSET2 loop-part BRANCH OFFSET
\\   where OFFSET points back to condition (the beginning) and OFFSET2 points to after the whole piece of code
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

\\ With the looping constructs, we can now write SPACES, which writes n spaces to stdout.
: SPACES
	BEGIN
		SPACE	\\ print a space
		1-	\\ until we count down to 0
		DUP 0=
	UNTIL
;

\\ .S prints the contents of the stack.  Very useful for debugging.
: .S
	DSP@		\\ get current stack pointer
	BEGIN
		DUP @ .		\\ print the stack element
		4+		\\ move up
		DUP S0 @ 4- =	\\ stop when we get to the top
	UNTIL
	DROP
;

\\ DEPTH returns the depth of the stack.
: DEPTH S0 @ DSP@ - ;

\\ .\" is the print string operator in FORTH.  Example: .\" Something to print\"
\\ The space after the operator is the ordinary space required between words.
\\ This is tricky to define because it has to do different things depending on whether
\\ we are compiling or in immediate mode.  (Thus the word is marked IMMEDIATE so it can
\\ detect this and do different things).
\\ In immediate mode we just keep reading characters and printing them until we get to
\\ the next double quote.
\\ In compile mode we have the problem of where we're going to store the string (remember
\\ that the input buffer where the string comes from may be overwritten by the time we
\\ come round to running the function).  We store the string in the compiled function
\\ like this:
\\   LITSTRING, string length, string rounded up to 4 bytes, EMITSTRING, ...
: .\" IMMEDIATE
	STATE @		\\ compiling?
	IF
		' LITSTRING ,	\\ compile LITSTRING
		HERE @		\\ save the address of the length word on the stack
		0 ,		\\ dummy length - we don't know what it is yet
		BEGIN
			KEY 		\\ get next character of the string
			DUP '\"' <>
		WHILE
			HERE @ !b	\\ store the character in the compiled image
			1 HERE +!	\\ increment HERE pointer by 1 byte
		REPEAT
		DROP		\\ drop the double quote character at the end
		DUP		\\ get the saved address of the length word
		HERE @ SWAP -	\\ calculate the length
		4-		\\ subtract 4 (because we measured from the start of the length word)
		SWAP !		\\ and back-fill the length location
		HERE @		\\ round up to next multiple of 4 bytes for the remaining code
		3 +
		3 INVERT AND
		HERE !
		' EMITSTRING ,	\\ compile the final EMITSTRING
	ELSE
		\\ In immediate mode, just read characters and print them until we get
		\\ to the ending double quote.  Much simpler than the above code!
		BEGIN
			KEY
			DUP '\"' = IF EXIT THEN
			EMIT
		AGAIN
	THEN
;

\\ While compiling, [COMPILE] WORD compiles WORD if it would otherwise be IMMEDIATE.
: [COMPILE] IMMEDIATE
	WORD		\\ get the next word
	FIND		\\ find it in the dictionary
	>CFA		\\ get its codeword
	,		\\ and compile that
;

\\ RECURSE makes a recursive call to the current word that is being compiled.
\\ Normally while a word is being compiled, it is marked HIDDEN so that references to the
\\ same word within are calls to the previous definition of the word.
: RECURSE IMMEDIATE
	LATEST @ >CFA	\\ LATEST points to the word being compiled at the moment
	,		\\ compile it
;

\\ ALLOT is used to allocate (static) memory when compiling.  It increases HERE by
\\ the amount given on the stack.
: ALLOT HERE +! ;


\\ Finally print the welcome prompt.
.\" OK \"
"

_initbufftop:
	.align 4096
buffend:

currkey:
	.int buffer
bufftop:
	.int _initbufftop
