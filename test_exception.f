( -*- text -*- )

: TEST4 PRINT-STACK-TRACE THROW ;

: TEST3 0 TEST4 26 TEST4 ;

: TEST2
	['] TEST3 CATCH
	?DUP IF ." TEST3 threw exception " . CR THEN
	TEST3
;

: TEST TEST2 ;
