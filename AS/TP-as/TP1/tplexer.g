grammar tplexer;
LETTER 	 	: 'a'..'f';
LETTRE	:	'A'..'F';
start	:	 R2 EOF| r2 EOF;
INT	:	('0'..'9');
R 	:	(INT)+| '-' + (INT)+;

r2	:	(INT)+|(LETTER)+;
R2	:	(INT)+|(LETTRE)+;