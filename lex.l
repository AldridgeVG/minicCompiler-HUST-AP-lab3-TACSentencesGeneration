%{
#include "parser.tab.h"
#include "string.h"
#include "stdio.h"
#include "define.h"

int yycolumn=1;
#define YY_USER_ACTION yylloc.first_line=yylloc.last_line=yylineno; \
	yylloc.first_column=yycolumn;\
    yylloc.last_column=yycolumn+yyleng-1;\
    yycolumn+=yyleng;

typedef union {
	int type_int;
	float type_float;
	char type_char;
	char type_id[32];
	struct node *ptr;
} YYLVAL;
#define YYSTYPE YYLVAL
%}

%option yylineno

int    [0-9]+
float  ([0-9]*\.[0-9]+)|([0-9]+\.)
char 	\'[0-9a-zA-Z]\'
id   [A-Za-z][A-Za-z0-9]*  

%%
{int}        {yylval.type_int=atoi(yytext); return INT;}
{float}      {yylval.type_float=atof(yytext); return FLOAT;}
{char}		 {yylval.type_char=yytext[1]; return CHAR;}
"int"        {strcpy(yylval.type_id,  yytext);return TYPE;}
"float"      {strcpy(yylval.type_id,  yytext);return TYPE;}
"char" 		 {strcpy(yylval.type_id,  yytext);return TYPE;}	
"break"		 {return BREAK;}
"continue"	{return CONTINUE;}
"return"     {return RETURN;}
"if"         {return IF;}
"else"       {return ELSE;}
"while"      {return WHILE;}
"for"		 {return FOR;}

{id}        {strcpy(yylval.type_id,  yytext); return ID;}
";"			{return SEMI;}
","			{return COMMA;}
">"|"<"|">="|"<="|"=="|"!=" {strcpy(yylval.type_id, yytext);;return RELOP;}
"="			{return ASSIGNOP;}
"+"			{return PLUS;}
"-"			{return MINUS;}
"*"			{return STAR;}
"/"			{return DIV;}
"++"		{return SELFADD;}
"--"		{return SELFDEC;}
"+="		{return ADD_ASSIGNOP;}
"-="		{return MINUS_ASSIGNOP;}
"*="		{return STAR_ASSIGNOP;}
"/="		{return DIV_ASSIGNOP;}
"&&"        {return AND;}
"||"        {return OR;}
"."         {return DOT;}
"!"			{return NOT;}
"("			{return LP;}
")"			{return RP;}
"["         {return LB;}
"]"         {return RB;}
"{"			{return LC;}
"}"			{return RC;}
[\n]        {yycolumn=1;}   
[ \r\t]     {}   
"//"		{char c=input();while(c!='\n') c=input();}
"/*"		{moreLinesComment();}
.			{printf("Error type A :Mysterious character \"%s\"\n\t at Line %d\n",yytext,yylineno);}

%%
int yywrap()
{
return 1;
}
int moreLinesComment()
{
	char c,c1;
loop:
	while((c=input())!='*'&&c!=0){}
	if((c1 = input())!='/'&&c!=0)
	{
		unput(c1);
		goto loop;
	}
	return 0;
}

