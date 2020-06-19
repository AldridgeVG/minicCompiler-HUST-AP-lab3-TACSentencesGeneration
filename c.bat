chcp 65001
win_flex lex.l
win_bison -d -v parser.y
gcc -o ana lex.yy.c parser.tab.c ast.c Analysis.c
ana check3.c
pause