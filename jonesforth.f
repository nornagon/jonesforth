\ -*- text -*-
\	A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems. -*- asm -*-
\	By Richard W.M. Jones <rich@annexia.org> http://annexia.org/forth
\	This is PUBLIC DOMAIN (see public domain release statement below).
\	$Id: jonesforth.f,v 1.12 2007-09-30 14:37:00 rich Exp $
\
\	The first part of this tutorial is in jonesforth.S.  Get if from http://annexia.org/forth
\
\	PUBLIC DOMAIN ----------------------------------------------------------------------
\
\	I, the copyright holder of this work, hereby release it into the public domain. This applies worldwide.
\
\	In case this is not legally possible, I grant any entity the right to use this work for any purpose,
\	without any conditions, unless such conditions are required by law.
\
\	SETTING UP ----------------------------------------------------------------------
\
\	Let's get a few housekeeping things out of the way.  Firstly because I need to draw lots of
\	ASCII-art diagrams to explain concepts, the best way to look at this is using a window which
\	uses a fixed width font and is at least this wide:
\
\<------------------------------------------------------------------------------------------------------------------------>
\
\	Secondly make sure TABS are set to 8 characters.  The following should be a vertical
\	line.  If not, sort out your tabs.
\
\	|
\       |
\	|
\
\	Thirdly I assume that your screen is at least 50 characters high.
\
\	START OF FORTH CODE ----------------------------------------------------------------------
\
\	We've now reached the stage where the FORTH system is running and self-hosting.  All further
\	words can be written as FORTH itself, including words like IF, THEN, .", etc which in most
\	languages would be considered rather fundamental.
\
\	Some notes about the code:
\
\	I use indenting to show structure.  The amount of whitespace has no meaning to FORTH however
\	except that you must use at least one whitespace character between words, and words themselves
\	cannot contain whitespace.
\
\	FORTH is case-sensitive.  Use capslock!

\ The primitive word /MOD (DIVMOD) leaves both the quotient and the remainder on the stack.  (On
\ i386, the idivl instruction gives both anyway).  Now we can define the / and MOD in terms of /MOD
\ and a few other primitives.
: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

\ Define some character constants
: '\n' 10 ;
: BL   32 ; \ BL (BLank) is a standard FORTH word for space.

\ CR prints a carriage return
: CR '\n' EMIT ;

\ SPACE prints a space
: SPACE BL EMIT ;

\ DUP, DROP are defined in assembly for speed, but this is how you might define them
\ in FORTH.  Notice use of the scratch variables _X and _Y.
\ : DUP _X ! _X @ _X @ ;
\ : DROP _X ! ;

\ The 2... versions of the standard operators work on pairs of stack entries.  They're not used
\ very commonly so not really worth writing in assembler.  Here is how they are defined in FORTH.
: 2DUP OVER OVER ;
: 2DROP DROP DROP ;

\ More standard FORTH words.
: 2* 2 * ;
: 2/ 2 / ;

\ NEGATE leaves the negative of a number on the stack.
: NEGATE 0 SWAP - ;

\ Standard words for booleans.
: TRUE  1 ;
: FALSE 0 ;
: NOT   0= ;

\ LITERAL takes whatever is on the stack and compiles LIT <foo>
: LITERAL IMMEDIATE
	' LIT ,		\ compile LIT
	,		\ compile the literal itself (from the stack)
	;

\ Now we can use [ and ] to insert literals which are calculated at compile time.  (Recall that
\ [ and ] are the FORTH words which switch into and out of immediate mode.)
\ Within definitions, use [ ... ] LITERAL anywhere that '...' is a constant expression which you
\ would rather only compute once (at compile time, rather than calculating it each time your word runs).
: ':'
	[		\ go into immediate mode (temporarily)
	CHAR :		\ push the number 58 (ASCII code of colon) on the parameter stack
	]		\ go back to compile mode
	LITERAL		\ compile LIT 58 as the definition of ':' word
;

\ A few more character constants defined the same way as above.
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

\ While compiling, '[COMPILE] word' compiles 'word' if it would otherwise be IMMEDIATE.
: [COMPILE] IMMEDIATE
	WORD		\ get the next word
	FIND		\ find it in the dictionary
	>CFA		\ get its codeword
	,		\ and compile that
;

\ RECURSE makes a recursive call to the current word that is being compiled.
\
\ Normally while a word is being compiled, it is marked HIDDEN so that references to the
\ same word within are calls to the previous definition of the word.  However we still have
\ access to the word which we are currently compiling through the LATEST pointer so we
\ can use that to compile a recursive call.
: RECURSE IMMEDIATE
	LATEST @	\ LATEST points to the word being compiled at the moment
	>CFA		\ get the codeword
	,		\ compile it
;

\	CONTROL STRUCTURES ----------------------------------------------------------------------
\
\ So far we have defined only very simple definitions.  Before we can go further, we really need to
\ make some control structures, like IF ... THEN and loops.  Luckily we can define arbitrary control
\ structures directly in FORTH.
\
\ Please note that the control structures as I have defined them here will only work inside compiled
\ words.  If you try to type in expressions using IF, etc. in immediate mode, then they won't work.
\ Making these work in immediate mode is left as an exercise for the reader.

\ condition IF true-part THEN rest
\	-- compiles to: --> condition 0BRANCH OFFSET true-part rest
\	where OFFSET is the offset of 'rest'
\ condition IF true-part ELSE false-part THEN
\ 	-- compiles to: --> condition 0BRANCH OFFSET true-part BRANCH OFFSET2 false-part rest
\	where OFFSET if the offset of false-part and OFFSET2 is the offset of rest

\ IF is an IMMEDIATE word which compiles 0BRANCH followed by a dummy offset, and places
\ the address of the 0BRANCH on the stack.  Later when we see THEN, we pop that address
\ off the stack, calculate the offset, and back-fill the offset.
: IF IMMEDIATE
	' 0BRANCH ,	\ compile 0BRANCH
	HERE @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
;

: THEN IMMEDIATE
	DUP
	HERE @ SWAP -	\ calculate the offset from the address saved on the stack
	SWAP !		\ store the offset in the back-filled location
;

: ELSE IMMEDIATE
	' BRANCH ,	\ definite branch to just over the false-part
	HERE @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
	SWAP		\ now back-fill the original (IF) offset
	DUP		\ same as for THEN word above
	HERE @ SWAP -
	SWAP !
;

\ BEGIN loop-part condition UNTIL
\	-- compiles to: --> loop-part condition 0BRANCH OFFSET
\	where OFFSET points back to the loop-part
\ This is like do { loop-part } while (condition) in the C language
: BEGIN IMMEDIATE
	HERE @		\ save location on the stack
;

: UNTIL IMMEDIATE
	' 0BRANCH ,	\ compile 0BRANCH
	HERE @ -	\ calculate the offset from the address saved on the stack
	,		\ compile the offset here
;

\ BEGIN loop-part AGAIN
\	-- compiles to: --> loop-part BRANCH OFFSET
\	where OFFSET points back to the loop-part
\ In other words, an infinite loop which can only be returned from with EXIT
: AGAIN IMMEDIATE
	' BRANCH ,	\ compile BRANCH
	HERE @ -	\ calculate the offset back
	,		\ compile the offset here
;

\ BEGIN condition WHILE loop-part REPEAT
\	-- compiles to: --> condition 0BRANCH OFFSET2 loop-part BRANCH OFFSET
\	where OFFSET points back to condition (the beginning) and OFFSET2 points to after the whole piece of code
\ So this is like a while (condition) { loop-part } loop in the C language
: WHILE IMMEDIATE
	' 0BRANCH ,	\ compile 0BRANCH
	HERE @		\ save location of the offset2 on the stack
	0 ,		\ compile a dummy offset2
;

: REPEAT IMMEDIATE
	' BRANCH ,	\ compile BRANCH
	SWAP		\ get the original offset (from BEGIN)
	HERE @ - ,	\ and compile it after BRANCH
	DUP
	HERE @ SWAP -	\ calculate the offset2
	SWAP !		\ and back-fill it in the original location
;

\	COMMENTS ----------------------------------------------------------------------
\
\ FORTH allows ( ... ) as comments within function definitions.  This works by having an IMMEDIATE
\ word called ( which just drops input characters until it hits the corresponding ).
: ( IMMEDIATE
	1		\ allowed nested parens by keeping track of depth
	BEGIN
		KEY		\ read next character
		DUP '(' = IF	\ open paren?
			DROP		\ drop the open paren
			1+		\ depth increases
		ELSE
			')' = IF	\ close paren?
				1-		\ depth decreases
			THEN
		THEN
	DUP 0= UNTIL		\ continue until we reach matching close paren, depth 0
	DROP		\ drop the depth counter
;

(
	From now on we can use ( ... ) for comments.

	STACK NOTATION ----------------------------------------------------------------------

	In FORTH style we can also use ( ... -- ... ) to show the effects that a word has on the
	parameter stack.  For example:

	( n -- )	means that the word consumes an integer (n) from the parameter stack.
	( b a -- c )	means that the word uses two integers (a and b, where a is at the top of stack)
				and returns a single integer (c).
	( -- )		means the word has no effect on the stack
)

( Some more complicated stack examples, showing the stack notation. )
: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) DUP ROT ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
	1+		( add one because of 'u' on the stack )
	4 *		( multiply by the word size )
	DSP@ +		( add to the stack pointer )
	@    		( and fetch )
;

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

( Standard words for manipulating BASE. )
: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;

(
	PRINTING NUMBERS ----------------------------------------------------------------------

	The standard FORTH word . (DOT) is very important.  It takes the number at the top
	of the stack and prints it out.  However first I'm going to implement some lower-level
	FORTH words:

	U.R	( u width -- )	which prints an unsigned number, padded to a certain width
	U.	( u -- )	which prints an unsigned number
	.R	( n width -- )	which prints a signed number, padded to a certain width.

	For example:
		-123 6 .R
	will print out these characters:
		<space> <space> - 1 2 3

	In other words, the number padded left to a certain number of characters.

	The full number is printed even if it is wider than width, and this is what allows us to
	define the ordinary functions U. and . (we just set width to zero knowing that the full
	number will be printed anyway).

	Another wrinkle of . and friends is that they obey the current base in the variable BASE.
	BASE can be anything in the range 2 to 36.

	While we're defining . &c we can also define .S which is a useful debugging tool.  This
	word prints the current stack (non-destructively) from top to bottom.
)

( This is the underlying recursive definition of U. )
: U.		( u -- )
	BASE @ /MOD	( width rem quot )
	?DUP IF			( if quotient <> 0 then )
		RECURSE		( print the quotient )
	THEN

	( print the remainder )
	DUP 10 < IF
		'0'		( decimal digits 0..9 )
	ELSE
		10 -		( hex and beyond digits A..Z )
		'A'
	THEN
	+
	EMIT
;

(
	FORTH word .S prints the contents of the stack.  It doesn't alter the stack.
	Very useful for debugging.
)
: .S		( -- )
	DSP@		( get current stack pointer )
	BEGIN
		DUP S0 @ <
	WHILE
		DUP @ U.	( print the stack element )
		SPACE
		4+		( move up )
	REPEAT
	DROP
;

( This word returns the width (in characters) of an unsigned number in the current base )
: UWIDTH	( u -- width )
	BASE @ /	( rem quot )
	?DUP IF		( if quotient <> 0 then )
		RECURSE 1+	( return 1+recursive call )
	ELSE
		1		( return 1 )
	THEN
;

: U.R		( u width -- )
	SWAP		( width u )
	DUP		( width u u )
	UWIDTH		( width u uwidth )
	-ROT		( u uwidth width )
	SWAP -		( u width-uwidth )
	( At this point if the requested width is narrower, we'll have a negative number on the stack.
	  Otherwise the number on the stack is the number of spaces to print.  But SPACES won't print
	  a negative number of spaces anyway, so it's now safe to call SPACES ... )
	SPACES
	( ... and then call the underlying implementation of U. )
	U.
;

(
	.R prints a signed number, padded to a certain width.  We can't just print the sign
	and call U.R because we want the sign to be next to the number ('-123' instead of '-  123').
)
: .R		( n width -- )
	SWAP		( width n )
	DUP 0< IF
		NEGATE		( width u )
		1		( save a flag to remember that it was negative | width n 1 )
		ROT		( 1 width u )
		SWAP		( 1 u width )
		1-		( 1 u width-1 )
	ELSE
		0		( width u 0 )
		ROT		( 0 width u )
		SWAP		( 0 u width )
	THEN
	SWAP		( flag width u )
	DUP		( flag width u u )
	UWIDTH		( flag width u uwidth )
	-ROT		( flag u uwidth width )
	SWAP -		( flag u width-uwidth )

	SPACES		( flag u )
	SWAP		( u flag )

	IF			( was it negative? print the - character )
		'-' EMIT
	THEN

	U.
;

( Finally we can define word . in terms of .R, with a trailing space. )
: . 0 .R SPACE ;

( The real U., note the trailing space. )
: U. U. SPACE ;

( ? fetches the integer at an address and prints it. )
: ? ( addr -- ) @ . ;

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

( DEPTH returns the depth of the stack. )
: DEPTH		( -- n )
	S0 @ DSP@ -
	4-			( adjust because S0 was on the stack when we pushed DSP )
;

(
	ALIGNED takes an address and rounds it up (aligns it) to the next 4 byte boundary.
)
: ALIGNED	( addr -- addr )
	3 + 3 INVERT AND	( (addr+3) & ~3 )
;

(
	ALIGN aligns the HERE pointer, so the next word appended will be aligned properly.
)
: ALIGN HERE @ ALIGNED HERE ! ;

(
	STRINGS ----------------------------------------------------------------------

	S" string" is used in FORTH to define strings.  It leaves the address of the string and
	its length on the stack, (length at the top of stack).  The space following S" is the normal
	space between FORTH words and is not a part of the string.

	This is tricky to define because it has to do different things depending on whether
	we are compiling or in immediate mode.  (Thus the word is marked IMMEDIATE so it can
	detect this and do different things).

	In compile mode we append
		LITSTRING <string length> <string rounded up 4 bytes>
	to the current word.  The primitive LITSTRING does the right thing when the current
	word is executed.

	In immediate mode there isn't a particularly good place to put the string, but in this
	case we put the string at HERE (but we _don't_ change HERE).  This is meant as a temporary
	location, likely to be overwritten soon after.
)
: S" IMMEDIATE		( -- addr len )
	STATE @ IF	( compiling? )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		BEGIN
			KEY 		( get next character of the string )
			DUP '"' <>
		WHILE
			HERE @ C!	( store the character in the compiled image )
			1 HERE +!	( increment HERE pointer by 1 byte )
		REPEAT
		DROP		( drop the double quote character at the end )
		DUP		( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		SWAP !		( and back-fill the length location )
		ALIGN		( round up to next multiple of 4 bytes for the remaining code )
	ELSE		( immediate mode )
		HERE @		( get the start address of the temporary space )
		BEGIN
			KEY
			DUP '"' <>
		WHILE
			OVER C!		( save next character )
			1+		( increment address )
		REPEAT
		DROP		( drop the final " character )
		HERE @ -	( calculate the length )
		HERE @		( push the start address )
		SWAP 		( addr len )
	THEN
;

(
	." is the print string operator in FORTH.  Example: ." Something to print"
	The space after the operator is the ordinary space required between words and is not
	a part of what is printed.

	In immediate mode we just keep reading characters and printing them until we get to
	the next double quote.

	In compile mode we use S" to store the string, then add TELL afterwards:
		LITSTRING <string length> <string rounded up to 4 bytes> TELL

	It may be interesting to note the use of [COMPILE] to turn the call to the immediate
	word S" into compilation of that word.  It compiles it into the definition of .",
	not into the definition of the word being compiled when this is running (complicated
	enough for you?)
)
: ." IMMEDIATE		( -- )
	STATE @ IF	( compiling? )
		[COMPILE] S"	( read the string, and compile LITSTRING, etc. )
		' TELL ,	( compile the final TELL )
	ELSE
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		BEGIN
			KEY
			DUP '"' = IF
				DROP	( drop the double quote character )
				EXIT	( return from this function )
			THEN
			EMIT
		AGAIN
	THEN
;

(
	CONSTANTS AND VARIABLES ----------------------------------------------------------------------

	In FORTH, global constants and variables are defined like this:

	10 CONSTANT TEN		when TEN is executed, it leaves the integer 10 on the stack
	VARIABLE VAR		when VAR is executed, it leaves the address of VAR on the stack

	Constants can be read but not written, eg:

	TEN . CR		prints 10

	You can read a variable (in this example called VAR) by doing:

	VAR @			leaves the value of VAR on the stack
	VAR @ . CR		prints the value of VAR
	VAR ? CR		same as above, since ? is the same as @ .

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

	Note for people reading the code below: DOCOL is a constant word which we defined in the
	assembler part which returns the value of the assembler symbol of the same name.
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
	a word is compiled that HERE has been left as a multiple of 4).
)
: ALLOT		( n -- addr )
	HERE @ SWAP	( here n )
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
	VALUES ----------------------------------------------------------------------

	VALUEs are like VARIABLEs but with a simpler syntax.  You would generally use them when you
	want a variable which is read often, and written infrequently.

	20 VALUE VAL 	creates VAL with initial value 20
	VAL		pushes the value directly on the stack
	30 TO VAL	updates VAL, setting it to 30

	Notice that 'VAL' on its own doesn't return the address of the value, but the value itself,
	making values simpler and more obvious to use than variables (no indirection through '@').
	The price is a more complicated implementation, although despite the complexity there is no
	performance penalty at runtime.

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
	PRINTING THE DICTIONARY ----------------------------------------------------------------------

	ID. takes an address of a dictionary entry and prints the word's name.

	For example: LATEST @ ID. would print the name of the last word that was defined.
)
: ID.
	4+		( skip over the link pointer )
	DUP C@		( get the flags/length byte )
	F_LENMASK AND	( mask out the flags - just want the length )

	BEGIN
		DUP 0>		( length > 0? )
	WHILE
		SWAP 1+		( addr len -- len addr+1 )
		DUP C@		( len addr -- len addr char | get the next character)
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
	C@		( get the flags/length byte )
	F_HIDDEN AND	( mask the F_HIDDEN flag and return it (as a truth value) )
;
: ?IMMEDIATE
	4+		( skip over the link pointer )
	C@		( get the flags/length byte )
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
		?DUP		( while link pointer is not null )
	WHILE
		DUP ?HIDDEN NOT IF	( ignore hidden words )
			DUP ID.		( but if not hidden, print the word )
		THEN
		SPACE
		@		( dereference the link pointer - go to previous word )
	REPEAT
	CR
;

(
	FORGET ----------------------------------------------------------------------

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
	DUMP ----------------------------------------------------------------------

	DUMP is used to dump out the contents of memory, in the 'traditional' hexdump format.

	Notice that the parameters to DUMP (address, length) are compatible with string words
	such as WORD and S".
)
: DUMP		( addr len -- )
	BASE @ ROT		( save the current BASE at the bottom of the stack )
	HEX			( and switch the hexadecimal mode )

	BEGIN
		DUP 0>		( while len > 0 )
	WHILE
		OVER 8 U.R	( print the address )
		SPACE

		( print up to 16 words on this line )
		2DUP		( addr len addr len )
		1- 15 AND 1+	( addr len addr linelen )
		BEGIN
			DUP 0>		( while linelen > 0 )
		WHILE
			SWAP		( addr len linelen addr )
			DUP C@		( addr len linelen addr byte )
			2 .R SPACE	( print the byte )
			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		REPEAT
		2DROP		( addr len )

		( print the ASCII equivalents )
		2DUP 1- 15 AND 1+ ( addr len addr linelen )
		BEGIN
			DUP 0>		( while linelen > 0)
		WHILE
			SWAP		( addr len linelen addr )
			DUP C@		( addr len linelen addr byte )
			DUP 32 128 WITHIN IF	( 32 <= c < 128? )
				EMIT
			ELSE
				DROP '.' EMIT
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

(
	CASE ----------------------------------------------------------------------

	CASE...ENDCASE is how we do switch statements in FORTH.  There is no generally
	agreed syntax for this, so I've gone for the syntax mandated by the ISO standard
	FORTH (ANS-FORTH).

	( some value on the stack )
	CASE
	test1 OF ... ENDOF
	test2 OF ... ENDOF
	testn OF ... ENDOF
	... ( default case )
	ENDCASE

	The CASE statement tests the value on the stack by comparing it for equality with
	test1, test2, ..., testn and executes the matching piece of code within OF ... ENDOF.
	If none of the test values match then the default case is executed.  Inside the ... of
	the default case, the value is still at the top of stack (it is implicitly DROP-ed
	by ENDCASE).  When ENDOF is executed it jumps after ENDCASE (ie. there is no "fall-through"
	and no need for a break statement like in C).

	The default case may be omitted.  In fact the tests may also be omitted so that you
	just have a default case, although this is probably not very useful.

	An example (assuming that 'q', etc. are words which push the ASCII value of the letter
	on the stack):

	0 VALUE QUIT
	0 VALUE SLEEP
	KEY CASE
		'q' OF 1 TO QUIT ENDOF
		's' OF 1 TO SLEEP ENDOF
		( default case: )
		." Sorry, I didn't understand key <" DUP EMIT ." >, try again." CR
	ENDCASE

	(In some versions of FORTH, more advanced tests are supported, such as ranges, etc.
	Other versions of FORTH need you to write OTHERWISE to indicate the default case.
	As I said above, this FORTH tries to follow the ANS FORTH standard).

	The implementation of CASE...ENDCASE is somewhat non-trivial.  I'm following the
	implementations from here:
	http://www.uni-giessen.de/faq/archiv/forthfaq.case_endcase/msg00000.html

	The general plan is to compile the code as a series of IF statements:

	CASE				(push 0 on the immediate-mode parameter stack)
	test1 OF ... ENDOF		test1 OVER = IF DROP ... ELSE
	test2 OF ... ENDOF		test2 OVER = IF DROP ... ELSE
	testn OF ... ENDOF		testn OVER = IF DROP ... ELSE
	... ( default case )		...
	ENDCASE				DROP THEN [THEN [THEN ...]]

	The CASE statement pushes 0 on the immediate-mode parameter stack, and that number
	is used to count how many THEN statements we need when we get to ENDCASE so that each
	IF has a matching THEN.  The counting is done implicitly.  If you recall from the
	implementation above of IF, each IF pushes a code address on the immediate-mode stack,
	and these addresses are non-zero, so by the time we get to ENDCASE the stack contains
	some number of non-zeroes, followed by a zero.  The number of non-zeroes is how many
	times IF has been called, so how many times we need to match it with THEN.

	This code uses [COMPILE] so that we compile calls to IF, ELSE, THEN instead of
	actually calling them while we're compiling the words below.

	As is the case with all of our control structures, they only work within word
	definitions, not in immediate mode.
)
: CASE IMMEDIATE
	0		( push 0 to mark the bottom of the stack )
;

: OF IMMEDIATE
	' OVER ,	( compile OVER )
	' = ,		( compile = )
	[COMPILE] IF	( compile IF )
	' DROP ,  	( compile DROP )
;

: ENDOF IMMEDIATE
	[COMPILE] ELSE	( ENDOF is the same as ELSE )
;

: ENDCASE IMMEDIATE
	' DROP ,	( compile DROP )

	( keep compiling THEN until we get to our zero marker )
	BEGIN
		?DUP
	WHILE
		[COMPILE] THEN
	REPEAT
;

(
	DECOMPILER ----------------------------------------------------------------------

	CFA> is the opposite of >CFA.  It takes a codeword and tries to find the matching
	dictionary definition.

	In this FORTH this is not so easy.  In fact we have to search through the dictionary
	because we don't have a convenient back-pointer (as is often the case in other versions
	of FORTH).

	This word returns 0 if it doesn't find a match.
)
: CFA>
	LATEST @	( start at LATEST dictionary entry )
	BEGIN
		?DUP		( while link pointer is not null )
	WHILE
		DUP >CFA	( cfa curr curr-cfa )
		2 PICK		( cfa curr curr-cfa cfa )
		= IF		( found a match? )
			NIP		( leave curr dictionary entry on the stack )
			EXIT		( and return from the function )
		THEN
		@		( follow link pointer back )
	REPEAT
	DROP		( restore stack )
	0		( sorry, nothing found )
;

(
	SEE decompiles a FORTH word.

	We search for the dictionary entry of the word, then search again for the next
	word (effectively, the end of the compiled word).  This results in two pointers:

	+---------+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 3 | T | E | N | DOCOL      | LIT        | 10         | EXIT       |
	+---------+---+---+---+---+------------+------------+------------+------------+
	 ^									       ^
	 |									       |
	Start of word							      End of word

	With this information we can have a go at decompiling the word.  We need to
	recognise "meta-words" like LIT, LITSTRING, BRANCH, etc. and treat those separately.
)
: SEE
	WORD FIND	( find the dictionary entry to decompile )

	( Now we search again, looking for the next word in the dictionary.  This gives us
	  the length of the word that we will be decompiling.  (Well, mostly it does). )
	HERE @		( address of the end of the last compiled word )
	LATEST @	( word last curr )
	BEGIN
		2 PICK		( word last curr word )
		OVER		( word last curr word curr )
		<>		( word last curr word<>curr? )
	WHILE			( word last curr )
		NIP		( word curr )
		DUP @		( word curr prev (which becomes: word last curr) )
	REPEAT

	DROP		( at this point, the stack is: start-of-word end-of-word )
	SWAP		( end-of-word start-of-word )

	( begin the definition with : NAME [IMMEDIATE] )
	':' EMIT SPACE DUP ID. SPACE
	DUP ?IMMEDIATE IF ." IMMEDIATE " THEN

	>DFA		( get the data address, ie. points after DOCOL | end-of-word start-of-data )

	( now we start decompiling until we hit the end of the word )
	BEGIN		( end start )
		2DUP >
	WHILE
		DUP @		( end start codeword )

		CASE
		' LIT OF		( is it LIT ? )
			4 + DUP @		( get next word which is the integer constant )
			.			( and print it )
		ENDOF
		' LITSTRING OF		( is it LITSTRING ? )
			[ CHAR S ] LITERAL EMIT '"' EMIT SPACE ( print S"<space> )
			4 + DUP @		( get the length word )
			SWAP 4 + SWAP		( end start+4 length )
			2DUP TELL		( print the string )
			'"' EMIT SPACE		( finish the string with a final quote )
			+ ALIGNED		( end start+4+len, aligned )
			4 -			( because we're about to add 4 below )
		ENDOF
		' 0BRANCH OF		( is it 0BRANCH ? )
			." 0BRANCH ( "
			4 + DUP @		( print the offset )
			.
			." ) "
		ENDOF
		' BRANCH OF		( is it BRANCH ? )
			." BRANCH ( "
			4 + DUP @		( print the offset )
			.
			." ) "
		ENDOF
		' ' OF			( is it ' (TICK) ? )
			[ CHAR ' ] LITERAL EMIT SPACE
			4 + DUP @		( get the next codeword )
			CFA>			( and force it to be printed as a dictionary entry )
			ID. SPACE
		ENDOF
		' EXIT OF		( is it EXIT? )
			( We expect the last word to be EXIT, and if it is then we don't print it
			  because EXIT is normally implied by ;.  EXIT can also appear in the middle
			  of words, and then it needs to be printed. )
			2DUP			( end start end start )
			4 +			( end start end start+4 )
			<> IF			( end start | we're not at the end )
				." EXIT "
			THEN
		ENDOF
					( default case: )
			DUP			( in the default case we always need to DUP before using )
			CFA>			( look up the codeword to get the dictionary entry )
			ID. SPACE		( and print it )
		ENDCASE

		4 +		( end start+4 )
	REPEAT

	';' EMIT CR

	2DROP		( restore stack )
;

(
	C STRINGS ----------------------------------------------------------------------

	FORTH strings are represented by a start address and length kept on the stack or in memory.

	Most FORTHs don't handle C strings, but we need them in order to access the process arguments
	and environment left on the stack by the Linux kernel.

	The main function we need is STRLEN which works out the length of a C string.  DUP STRLEN is
	a common idiom which 'converts' a C string into a FORTH string.  (For example, DUP STRLEN TELL
	prints a C string).
)

(
	Z" .." is like S" ..." except that the string is terminated by an ASCII NUL character.

	To make it more like a C string, at runtime Z" just leaves the address of the string
	on the stack (not address & length as with S").  To implement this we need to add the
	extra NUL to the string and also a DROP instruction afterwards.  Apart from that the
	implementation just a modified S".
)
: Z" IMMEDIATE
	STATE @ IF	( compiling? )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		BEGIN
			KEY 		( get next character of the string )
			DUP '"' <>
		WHILE
			HERE @ C!	( store the character in the compiled image )
			1 HERE +!	( increment HERE pointer by 1 byte )
		REPEAT
		0 HERE @ C!	( add the ASCII NUL byte )
		1 HERE +!
		DROP		( drop the double quote character at the end )
		DUP		( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		SWAP !		( and back-fill the length location )
		ALIGN		( round up to next multiple of 4 bytes for the remaining code )
		' DROP ,	( compile DROP (to drop the length) )
	ELSE		( immediate mode )
		HERE @		( get the start address of the temporary space )
		BEGIN
			KEY
			DUP '"' <>
		WHILE
			OVER C!		( save next character )
			1+		( increment address )
		REPEAT
		DROP		( drop the final " character )
		0 SWAP C!	( store final ASCII NUL )
		HERE @		( push the start address )
	THEN
;

( STRLEN returns the length of a C string )
: STRLEN 	( str -- len )
	DUP		( save start address )
	BEGIN
		DUP C@ 0<>	( zero byte found? )
	WHILE
		1+
	REPEAT

	SWAP -		( calculate the length )
;

(
	STRNCMP compares two strings up to a length.  As with C's strncmp it returns 0 if they
	are equal, or a number > 0 or < 0 indicating their order.
)
: STRNCMP	( str1 str2 len -- eq? )
	BEGIN
		?DUP
	WHILE
		ROT		( len str1 str2 )
		DUP C@		( len str1 str2 char2 )
		2 PICK C@	( len str1 str2 char2 char1 )
		OVER   		( len str1 str2 char2 char1 char2 )
		-		( len str1 str2 char2 char1-char2 )

		?DUP IF		( strings not the same at this position? )
			NIP		( len str1 str2 diff )
			ROT		( len diff str1 str2 )
			DROP DROP	( len diff )
			NIP  		( diff )
			EXIT
		THEN

		0= IF		( characters are equal, but is this the end of the C string? )
			DROP DROP DROP
			0
			EXIT
		THEN

		1+		( len str1 str2+1 )
		ROT		( str2+1 len str1 )
		1+ ROT		( str1+1 str2+1 len )
		1-		( str1+1 str2+1 len-1 )
	REPEAT

	2DROP		( restore stack )
	0		( equal )
;

(
	THE ENVIRONMENT ----------------------------------------------------------------------

	Linux makes the process arguments and environment available to us on the stack.

	The top of stack pointer is saved by the early assembler code when we start up in the FORTH
	variable S0, and starting at this pointer we can read out the command line arguments and the
	environment.

	Starting at S0, S0 itself points to argc (the number of command line arguments).

	S0+4 points to argv[0], S0+8 points to argv[1] etc up to argv[argc-1].

	argv[argc] is a NULL pointer.

	After that the stack contains environment variables, a set of pointers to strings of the
	form NAME=VALUE and on until we get to another NULL pointer.

	The first word that we define, ARGC, pushes the number of command line arguments (note that
	as with C argc, this includes the name of the command).
)
: ARGC
	S0 @ @
;

(
	n ARGV gets the nth command line argument.

	For example to print the command name you would do:
		0 ARGV TELL CR
)
: ARGV ( n -- str u )
	1+ CELLS S0 @ +	( get the address of argv[n] entry )
	@		( get the address of the string )
	DUP STRLEN	( and get its length / turn it into a FORTH string )
;

(
	ENVIRON returns the address of the first environment string.  The list of strings ends
	with a NULL pointer.

	For example to print the first string in the environment you could do:
		ENVIRON @ DUP STRLEN TELL
)
: ENVIRON	( -- addr )
	ARGC		( number of command line parameters on the stack to skip )
	2 +		( skip command line count and NULL pointer after the command line args )
	CELLS		( convert to an offset )
	S0 @ +		( add to base stack address )
;

(
	SYSTEM CALLS ----------------------------------------------------------------------

	Some wrappers around Linux system calls
)

( BYE exits by calling the Linux exit(2) syscall. )
: BYE		( -- )
	0
	0
	0		( return code (0) )
	SYS_EXIT	( system call number )
	SYSCALL3
;

(
	OPEN, CREAT and CLOSE are just like the Linux syscalls open(2), creat(2) and close(2).

	Notice that they take C strings and may return error codes (-errno).
)
: OPEN		( mode flags c-pathname -- ret )
	SYS_OPEN
	SYSCALL3
;

: CREAT		( mode c-pathname -- ret )
	0 ROT
	SYS_CREAT
	SYSCALL3
;

: CLOSE		( fd -- ret )
	0 SWAP 0 ROT
	SYS_CLOSE
	SYSCALL3
;

( READ and WRITE system calls. )
: READ		( len buffer fd -- ret )
	SYS_READ
	SYSCALL3
;	

: WRITE		( len buffer fd -- ret )
	SYS_WRITE
	SYSCALL3
;	

(
	ANS FORTH ----------------------------------------------------------------------

	From this point we're trying to fill in the missing parts of the ISO standard, commonly
	referred to as ANS FORTH.

	http://www.taygeta.com/forth/dpans.html
	http://www.taygeta.com/forth/dpansf.htm (list of words)
)

( C, writes a byte at the HERE pointer. )
: C, HERE @ C! 1 HERE +! ;









(
	NOTES ----------------------------------------------------------------------

	DOES> isn't possible to implement with this FORTH because we don't have a separate
	data pointer.
)

(
	WELCOME MESSAGE ----------------------------------------------------------------------

	Print the version and OK prompt.
)

." JONESFORTH VERSION " VERSION . CR
." OK "
