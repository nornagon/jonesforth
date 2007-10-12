( -*- text -*-
  FORTH repeated DUP DROP * 1000 using ordinary indirect threaded code
  and the assembler primitives.
  $Id: perf_dupdrop.f,v 1.3 2007-10-12 01:46:26 rich Exp $ )

1024 32 * MORECORE

( Print the time passed. )
: PRINT-TIME	( lsb msb lsb msb -- lsb lsb )
	( The test is very short so likely the MSBs will be the same.  This
	  makes calculating the time easier (because we can only do 32 bit
	    subtraction).  So check MSBs are equal. )
	2 PICK <> IF
		." MSBs not equal, please repeat the test" CR
	ELSE
		NIP
		SWAP - U. CR
	THEN
;

: 4DROP DROP DROP DROP DROP ;

: PERFORM-TEST	( xt -- )
	( Get everything in the cache. )
	DUP EXECUTE 4DROP
	DUP EXECUTE 4DROP
	DUP EXECUTE 4DROP
	DUP EXECUTE 4DROP
	DUP EXECUTE 4DROP
	DUP EXECUTE 4DROP
	0 0 0 0 PRINT-TIME
	( Run the test 10 times. )
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DUP EXECUTE PRINT-TIME
	DROP
;

( ---------------------------------------------------------------------- )
( Make a word which builds the repeated DUP DROP sequence. )
: MAKE-DUPDROP	( n -- )
	BEGIN ?DUP WHILE ' DUP , ' DROP , 1- REPEAT
;

( Now the actual test routine. )
: TEST		( -- startlsb startmsb endlsb endmsb )
	RDTSC			( Start time )
	[ 1000 MAKE-DUPDROP ]	( 1000 * DUP DROP )
	RDTSC			( End time )
;

: RUN ['] TEST PERFORM-TEST ;
RUN

( ---------------------------------------------------------------------- )
( Try the inlined alternative. )

( Inline the assembler primitive (cfa) n times. )
: *(INLINE) ( cfa n -- )
	BEGIN ?DUP WHILE OVER (INLINE) 1- REPEAT DROP
;

: DUPDROP INLINE DUP INLINE DROP ;CODE

: TEST
	INLINE RDTSC
	[ S" DUPDROP" FIND >CFA 1000 *(INLINE) ]
	INLINE RDTSC
;CODE

: RUN ['] TEST PERFORM-TEST ;
RUN
