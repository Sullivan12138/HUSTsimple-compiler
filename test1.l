%{
#include "test1.tab.h"
#include "string.h"
#include "def.h"
char parse(char *);
void parseString(char *,char *);
int yycolumn=1;
#define YY_USER_ACTION    	yylloc.first_line=yylloc.last_line=yylineno; \
	yylloc.first_column=yycolumn;	yylloc.last_column=yycolumn+yyleng-1; yycolumn+=yyleng;
typedef union {
	int type_int;
	int type_float;
    char type_char;
	char type_string[20];
	char type_id[32];
	struct node *ptr;
} YYLVAL;
#define YYSTYPE YYLVAL

%}
%option yylineno

id   [A-Za-z][A-Za-z0-9]*  
int    [0-9]+
float  ([0-9]*\.[0-9]+)|([0-9]+\.)
char 	'.'|'\\n'|'\\t'|'\\\\'|'\\\''|'\\\"'|'\\0'|''
string  \"(.)*\"
comments \/\*(.|\n)*\*\/
comment \/\/(.)*
%%
{int}        {yylval.type_int=atoi(yytext); return INT;}
{float}      {yylval.type_float=atof(yytext); return FLOAT;}
{char}       {yylval.type_char=parse(yytext); return CHAR;}
{string}     {parseString(yytext,yylval.type_string);return STRING;}
"int"        {strcpy(yylval.type_id,  yytext);return TYPE;}
"float"      {strcpy(yylval.type_id,  yytext);return TYPE;}
"char"       {strcpy(yylval.type_id, yytext);return TYPE;}

"return"     {return RETURN;}
"if"         {return IF;}
"else"       {return ELSE;}
"while"      {return WHILE;}
"break"      {return BREAK;}
"struct"     {return STRUCT;}
"for"        {return FOR;}
"continue"   {return CONTINUE;}

{id}       {strcpy(yylval.type_id,  yytext); return ID;/*由于关键字的形式也符合表示符的规则，所以把关键字的处理全部放在标识符的前面，优先识别*/}
";"			{return SEMI;}
","			{return COMMA;}
">"|"<"|">="|"<="|"=="|"!=" {strcpy(yylval.type_id, yytext);;return RELOP;}
"="			{return ASSIGNOP;}
"+"			{return PLUS;}
"-"			{return MINUS;}
"*"			{return STAR;}
"/"			{return DIV;}
"&&"         {return AND;}
"||"            {return OR;}
"!"			{return NOT;}
"("			{return LP;}
")"			{return RP;}
"{"			{return LC;}
"}"			{return RC;}
[\n]           {yycolumn=1;}   
[ \r\t]          {}
"["         {return LM;}
"]"         {return RM;}
"++"        {return INCREMENT;}
"--"        {return DECREMENT;}
"."         {return DOT;}
{comment}   {}
{comments}  {}         
.			{printf("Error type A :Mysterious character \"%s\"\n\t at Line %d\n",yytext,yylineno);}

%%


char parse(char *str) {
	if (strlen(str) == 4) {
		switch(str[2]) {
			case 'n': return '\n';
			case 't': return '\t';
			case '0': return '\0';
			case '\\': return '\\';
			case '\"': return '\"';
			case '\'': return '\'';
			default: return '\0';
		}
	} else if (strlen(str) == 2) {
		return '\0';
	} else {
		return str[1];
	}
}
void parseString(char *str1, char*str2) {
	int i = 0;
	for (i = 1; i < strlen(str1) - 1; i++) {
		str2[i-1] = str1[i];
	}
	str2[i] = '\0';
}
/* 和bison联用时，不需要这部分
void main()
{
yylex();
return 0;
}

*/

int yywrap()
{
return 1;
}
