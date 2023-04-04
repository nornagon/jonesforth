(
    allison's jonesforth extension. contains a few utility functions and a horrible softfloat implementation
    this also requires an extension to the asm source for the bitshift words
)


(
	slices a string, rust-style
)
: SLICEUNCHECKED ( addr0 len0 start end -- addr0 len0 addr1 len1 )
	OVER ( a0 l0 s e s )
	- ( a0 l0 s l1 )
	3 PICK ( a0 l0 s l1 a0 )
	ROT + SWAP ( a0 l0 a1 l1 )
;
: SLICE ( addr0 len0 start end -- addr0 len0 addr1 len1 success )
	\ helper method for enforcing safety invariants
	\ will always put 5 values on the stack, but addr1 and len1 may be junk if it fails
	\ success is 0 on success and 1 on any failure
	\ check if start is greater than end
	OVER OVER > \ a0 l0 s e s>e
	IF 0 0 1 EXIT THEN \ exit if start>end
	\ check if end > l0 
	( a0 l0 s e ) 2 PICK \ a0 l0 s e l0
	OVER < IF 0 0 1 EXIT THEN \ exit if end > l0
	SLICEUNCHECKED 0
;
HIDE SLICEUNCHECKED \ MEMORY SAFETY

: D. DUP . ;
: DTELL OVER OVER TELL ;
: PUT ( x_u ... x_1 x_0 y u -- y ... x_1 x_0)
	2 + 4 * DSP@ + !
;
: EXITWITH SYS_EXIT SYSCALL1 ;

( 
	soft floating point primatives
	floats are split into 3 values: sign, exp and mant
	mant will have an explicit leading 1 while doing arith, and implicit when merging back to f32
)

: XEXP \ extract exponent
	23 >>U 255 AND
;
: XMANT \ extract mantissa
	8388607 AND 8388608 + \ explicit leading 1
;
: ISVMANT ( mant -- err )
	(
		check if a dword is a valid float mantissa
		returns 0 if valid, 1 if too big, 2 if too small
	)
	DUP [ 16777215 INVERT ] LITERAL AND \ get bits that would be above mantissa
	\ if all are zero, keep going, else push 1 then return
	0<> IF DROP 1 EXIT THEN 
	8388608 AND \ extract top bit of mantissa
	\ if zero, 
	0= IF 2 EXIT THEN
	0
;
: I2FL ( int -- sign exp mant )
	(
		mant will have implicit leading 1
	)
	DUP 2147483648 AND \ extract sign bit
	0<> IF NEGATE 1 ELSE 0 THEN -ROT \ work out sign for float, and invert int
	[ 23 127 + ] LITERAL \ shift
	BEGIN OVER ISVMANT DUP 0<> WHILE \ do loop if not valid, leave err on stack
		\." NOT VALID MANT: " ROT DUP . -ROT SPACE
		( i exp err )
		1 = IF \ too big
			\." TOO LARGE " CR
			1+ SWAP 1 >>U SWAP \ inc exp, shift i
		ELSE
			\." TOO SMALL " CR
			1- SWAP 1 <<U SWAP \ dec exp, shift i
		THEN
		\OVER .
	REPEAT
	DROP \ remove err
	SWAP 8388607 AND
;
: MKFLT ( sign exp mant -- f32 )
	SWAP 23 <<U OR SWAP \ merge exp and mant, get sign on top to manipulate
    IF 2147483648 OR THEN \ if negative, put sign on top
;
: SPFLT ( f32 -- sign exp mant )
    DUP 2147483648 AND 0<> SWAP \ extract sign
    DUP XEXP
    SWAP XMANT
    ( sign exp mant_p )
    OVER 0= IF 8388607 AND THEN \ if exp all zero, denormal hence no leading 1
;
