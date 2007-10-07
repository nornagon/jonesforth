( -*- text -*- )

: TEST
	123                                . CR
	[ HEX -7F ] LITERAL      DECIMAL   . CR
	[ HEX 7FF77FF7 ] LITERAL HEX       . CR
	[ HEX -7FF77FF7 ] LITERAL 2 BASE ! . CR
	[ 2 BASE ! 1111111111101110111111111110111 ] LITERAL HEX . CR
;

DECIMAL ( restore immediate-mode base )
