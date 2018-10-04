parser: lex.yy.c y.tab.c
	gcc -o parser lex.yy.c y.tab.c -ly -ll
lex.yy.c: scanner.l
	lex scanner.l
y.tab.c: parser.y
	yacc -d -v parser.y
