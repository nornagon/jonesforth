( -*- text -*- )

: TEST
	DEPTH . CR

	42 DUP . . CR
	23 DROP DEPTH . CR
	1 2 SWAP . . CR
	1 2 OVER . . . CR
	1 2 3 -ROT . . . CR
	1 2 3 ROT . . . CR
	1 2 3 4 2DROP . . CR
	1 2 3 4 2DUP . . . . . . CR
	1 2 3 4 2SWAP . . . . CR

	DEPTH . CR
;
