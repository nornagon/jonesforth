/* A sometimes minimal FORTH interpreter for Linux / i386 systems. -*- asm -*-
 * By Richard W.M. Jones <rich@annexia.org>
 *
 * gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o jonesforth jonesforth.S
 */

#include <asm-i386/unistd.h>

/* NOTES-------------------------------------------------------------------------------------------------------------------

Need to say something about $ before constants.

And about je/jne/ja/jb/jbe/etc




*/

/* NEXT macro. */
	.macro NEXT
	lodsl
	jmp *(%eax)
	.endm

/* Macros to deal with the return stack. */
	.macro PUSHRSP reg
	lea -4(%ebp),%ebp	// push reg on to return stack
	movl \reg,(%ebp)
	.endm

	.macro POPRSP reg
	mov (%ebp),\reg		// pop top of return stack to reg
	lea 4(%ebp),%ebp
	.endm

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

/* DOCOL - the interpreter! */
	.text
	.align 4
DOCOL:
	PUSHRSP %esi		// push %esi on to the return stack
	addl $4,%eax		// %eax points to codeword, so make
	movl %eax,%esi		// %esi point to first data word
	NEXT

/*----------------------------------------------------------------------
 * Fixed sized buffers for everything.
 */
	.bss

/* FORTH return stack. */
#define RETURN_STACK_SIZE 8192
	.align 4096
	.space RETURN_STACK_SIZE
return_stack:

/* Space for user-defined words. */
#define USER_DEFS_SIZE 16384
	.align 4096
user_defs_start:
	.space USER_DEFS_SIZE






/*----------------------------------------------------------------------
 * Built-in words defined the long way.
 */
#define F_IMMED 0x80
#define F_HIDDEN 0x20

	// Store the chain of links.
	.set link,0

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

	.macro defvar name, namelen, flags=0, label, initial=0
	defcode \name,\namelen,\flags,\label
	push $var_\name
	NEXT
	.data
	.align 4
var_\name :
	.int \initial
	.endm

	// Some easy ones, written in assembly for speed
	defcode "DROP",4,,DROP
	pop %eax		// drop top of stack
	NEXT

	defcode "DUP",3,,DUP
	pop %eax		// duplicate top of stack
	push %eax
	push %eax
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

	defcode "+!",2,ADDSTORE
	pop %ebx		// address
	pop %eax		// the amount to add
	addl %eax,(%ebx)	// add it
	NEXT

	defcode "-!",2,SUBSTORE
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
			HERE 1 +!	\\ increment HERE pointer by 1 byte
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
