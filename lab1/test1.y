%error-verbose
%locations
%{
#include "stdio.h"
#include "math.h"
#include "string.h"
#include "def.h"
int yyparse(void);
int yylex(void);
extern int yylineno;
extern char *yytext;
extern FILE *yyin;
void yyerror(const char* fmt, ...);
void display(struct node *,int);
%}

%union {
	int    type_int;
	float  type_float;
        char type_char;
        char type_string[20];
	char   type_id[32];
	struct node *ptr;
};

//  %type 定义非终结符的语义值类型
%type  <ptr> program ExtDefList ExtDef  Specifier ExtDecList FuncDec CompSt VarList VarDec ParamDec Stmt StmList DefList Def DecList Dec Exp Args
%type  <ptr> StructDec ExtArrayList ForCondition StructDefList  StructList ExtStructList ArrayDec StructDef ArrayList
//% token 定义终结符的语义值类型
%token <type_int> INT              //指定INT的语义值是type_int，有词法分析得到的数值
%token <type_id> ID RELOP TYPE  //指定ID,RELOP 的语义值是type_id，有词法分析得到的标识符字符串
%token <type_float> FLOAT         //指定ID的语义值是type_id，有词法分析得到的标识符字符串
%token <type_char> CHAR
%token <type_string> STRING

%token LP RP LC RC SEMI COMMA LM RM  //用bison对该文件编译时，带参数-d，生成的exp.tab.h中给这些单词进行编码，可在lex.l中包含parser.tab.h使用这些单词种类码
%token PLUS MINUS STAR DIV ASSIGNOP AND OR NOT IF ELSE WHILE RETURN
%token INCREMENT DECREMENT COMMENT FOR DOT BREAK CONTINUE STRUCT

%left ASSIGNOP
%left OR
%left AND
%left RELOP
%left PLUS MINUS
%left STAR DIV
%right UMINUS NOT

%nonassoc LOWER_THEN_ELSE
%nonassoc ELSE

%%

program: ExtDefList    { display($1,0); }     /*显示语法树,语义分析*/
         ; 
//外部语句定义
ExtDefList: {$$=NULL;}
          | ExtDef ExtDefList {$$=mknode(EXT_DEF_LIST,$1,$2,NULL,yylineno);}   //每一个EXTDEFLIST的结点，其第1棵子树对应一个外部变量声明或函数
          ;  
//具体的单个外部语句
ExtDef:   Specifier ExtDecList SEMI   {$$=mknode(EXT_VAR_DEF,$1,$2,NULL,yylineno);}   //该结点对应一个外部变量声明
         |Specifier FuncDec CompSt    {$$=mknode(FUNC_DEF,$1,$2,$3,yylineno);}         //该结点对应一个函数定义
         |ExtStructList SEMI {$$=mknode(EXT_STRUCT_DEF,$1,NULL,NULL,yylineno);} //该节点对应一个结构体定义
         |Specifier ExtArrayList SEMI {$$=mknode(EXT_ARR_DEF,$1,$2,NULL,yylineno);}
         | error SEMI   {$$=NULL; }
         ;
//类型
Specifier:  TYPE    {$$=mknode(TYPE,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);$$->type=!strcmp($1,"int")?INT:FLOAT;} 
                |  STRUCT ID {$$=mknode(TYPE,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$2);}
           ; 
//外部变量列表     
ExtDecList:  VarDec      {$$=$1;}       /*每一个EXT_DECLIST的结点，其第一棵子树对应一个变量名(ID类型的结点),第二棵子树对应剩下的外部变量名*/
           | VarDec COMMA ExtDecList {$$=mknode(EXT_DEC_LIST,$1,$3,NULL,yylineno);}
           ; 
//外部数组列表
ExtArrayList: ArrayDec   {$$=$1;}
            | ArrayDec COMMA ExtArrayList{$$=mknode(ARR_LIST,$1,$3,NULL,yylineno);};
//外部结构体列表
ExtStructList: StructDec {$$=$1;}
            | StructDec COMMA ExtStructList{$$=mknode(STRUCT_LIST,$1,$3,NULL,yylineno);};
//变量名字
VarDec:  ID          {$$=mknode(ID,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);}   //ID结点，标识符符号串存放结点的type_id
         ;
//数组名字
ArrayDec:  ID LM Exp RM {$$=mknode(ARRAY_DEC,$3,NULL,NULL,yylineno);strcpy($$->type_id,$1);};
//结构体名字
StructDec: STRUCT ID LC StructDefList RC {$$=mknode(STRUCT_DEC,$4,NULL,NULL,yylineno);strcpy($$->type_id,$2);};
//函数头
FuncDec: ID LP VarList RP   {$$=mknode(FUNC_DEC,$3,NULL,NULL,yylineno);strcpy($$->type_id,$1);}//函数名存放在$$->type_id
		|ID LP  RP   {$$=mknode(FUNC_DEC,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);}//函数名存放在$$->type_id

        ; 
//函数形参列表 
VarList: ParamDec  {$$=mknode(PARAM_LIST,$1,NULL,NULL,yylineno);}
        | ParamDec COMMA  VarList  {$$=mknode(PARAM_LIST,$1,$3,NULL,yylineno);}
        ;
//单个函数形参
ParamDec: Specifier VarDec         {$$=mknode(PARAM_DEC,$1,$2,NULL,yylineno);}
         ;
//复合语句，变量必须在语句之前
CompSt: LC DefList StmList RC    {$$=mknode(COMP_STM,$2,$3,NULL,yylineno);}
       ;
//语句列表
StmList: {$$=NULL; }  
        | Stmt StmList  {$$=mknode(STM_LIST,$1,$2,NULL,yylineno);}
        ;
//单个语句
Stmt:   Exp SEMI    {$$=mknode(EXP_STMT,$1,NULL,NULL,yylineno);}
      | CompSt      {$$=$1;}      //复合语句结点直接最为语句结点，不再生成新的结点
      | RETURN Exp SEMI   {$$=mknode(RETURN,$2,NULL,NULL,yylineno);}
      | IF LP Exp RP Stmt %prec LOWER_THEN_ELSE   {$$=mknode(IF_THEN,$3,$5,NULL,yylineno);}
      | IF LP Exp RP Stmt ELSE Stmt   {$$=mknode(IF_THEN_ELSE,$3,$5,$7,yylineno);}
      | WHILE LP Exp RP Stmt {$$=mknode(WHILE,$3,$5,NULL,yylineno);} 
      | FOR LP ForCondition RP Stmt{$$=mknode(FOR,$3,$5,NULL,yylineno);}
      ;
//结构体内部的变量成员
StructDefList: {$$=NULL;}
        | StructDef StructDefList{$$=mknode(DEF_LIST,$1,$2,NULL,yylineno);};
//局部变量列表的列表  
DefList: {$$=NULL; }
        | Def DefList {$$=mknode(DEF_LIST,$1,$2,NULL,yylineno);}
        ;
//单个局部变量
Def:    Specifier DecList SEMI {$$=mknode(VAR_DEF,$1,$2,NULL,yylineno);}
        | Specifier ArrayList SEMI{$$=mknode(ARRAY_DEF,$1,$2,NULL,yylineno);}
        ;
//单个结构体变量列表
StructDef: Specifier StructList SEMI{$$=mknode(VAR_DEF,$1,$2,NULL,yylineno);};
//同类型变量的声明列表
DecList: Dec  {$$=mknode(DEC_LIST,$1,NULL,NULL,yylineno);}
       | Dec COMMA DecList  {$$=mknode(DEC_LIST,$1,$3,NULL,yylineno);}
	   ;
//内部数组列表
ArrayList: ArrayDec   {$$=$1;}
            | ArrayDec COMMA ArrayList{$$=mknode(ARR_LIST,$1,$3,NULL,yylineno);};
//同结构体类型变量声明列表
StructList: VarDec{$$=mknode(DEC_LIST,$1,NULL,NULL,yylineno);}  
                |VarDec COMMA StructList {$$=mknode(DEC_LIST,$1,$3,NULL,yylineno);};
//单个局部变量，与外部不同之处在于可以用表达式初始化
Dec:     VarDec  {$$=$1;}
       | VarDec ASSIGNOP Exp  {$$=mknode(ASSIGNOP,$1,$3,NULL,yylineno);strcpy($$->type_id,"ASSIGNOP");}
       ;
//for循环条件体
ForCondition: Exp SEMI Exp SEMI Exp{$$=mknode(FOR_CONDITION,$1,$3,$5,yylineno);strcpy($$->type_id,"FOR_CONDITION");};
//表达式
Exp:    Exp ASSIGNOP Exp {$$=mknode(ASSIGNOP,$1,$3,NULL,yylineno);strcpy($$->type_id,"ASSIGNOP");}//$$结点type_id空置未用，正好存放运算符
      | Exp AND Exp   {$$=mknode(AND,$1,$3,NULL,yylineno);strcpy($$->type_id,"AND");}
      | Exp OR Exp    {$$=mknode(OR,$1,$3,NULL,yylineno);strcpy($$->type_id,"OR");}
      | Exp RELOP Exp {$$=mknode(RELOP,$1,$3,NULL,yylineno);strcpy($$->type_id,$2);}  //词法分析关系运算符号自身值保存在$2中
      | Exp PLUS Exp  {$$=mknode(PLUS,$1,$3,NULL,yylineno);strcpy($$->type_id,"PLUS");}
      | Exp MINUS Exp {$$=mknode(MINUS,$1,$3,NULL,yylineno);strcpy($$->type_id,"MINUS");}
      | Exp STAR Exp  {$$=mknode(STAR,$1,$3,NULL,yylineno);strcpy($$->type_id,"STAR");}
      | Exp DIV Exp   {$$=mknode(DIV,$1,$3,NULL,yylineno);strcpy($$->type_id,"DIV");}
      | LP Exp RP     {$$=$2;}
      | MINUS Exp %prec UMINUS   {$$=mknode(UMINUS,$2,NULL,NULL,yylineno);strcpy($$->type_id,"UMINUS");}
      | NOT Exp       {$$=mknode(NOT,$2,NULL,NULL,yylineno);strcpy($$->type_id,"NOT");}
      | ID LP Args RP {$$=mknode(FUNC_CALL,$3,NULL,NULL,yylineno);strcpy($$->type_id,$1);}
      | ID LP RP      {$$=mknode(FUNC_CALL,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);}//函数访问
      | ID            {$$=mknode(ID,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);}
      | INT           {$$=mknode(INT,NULL,NULL,NULL,yylineno);$$->type_int=$1;$$->type=INT;}
      | FLOAT         {$$=mknode(FLOAT,NULL,NULL,NULL,yylineno);$$->type_float=$1;$$->type=FLOAT;}
      | CHAR          {$$=mknode(CHAR,NULL,NULL,NULL,yylineno);$$->type_char=$1;$$->type=CHAR;}
      | STRING        {$$=mknode(STRING,NULL,NULL,NULL,yylineno);strcpy($$->type_string,$1);$$->type=STRING;}
      | ID INCREMENT  {$$=mknode(INCREMENT,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);}
      | ID DECREMENT  {$$=mknode(DECREMENT,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);}
      | ID DOT ID     {struct node*t1=mknode(ID,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);
                        struct node*t2=mknode(ID,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$3);
                        $$=mknode(STRUCT_VAR,t1,t2,NULL,yylineno);char t[33];strcpy(t,$1);strcat(t,".");strcat(t,$3);strcpy($$->type_id,t);}
      | ID LM Exp RM {$$=mknode(ARRAY_VAR,$3,NULL,NULL,yylineno);strcpy($$->type_id,$1);} //数组访问
      | BREAK {$$=mknode(BREAK,NULL,NULL,NULL,yylineno);}
      | CONTINUE {$$=mknode(CONTINUE,NULL,NULL,NULL,yylineno);}
      ;
//函数实参列表
Args:    Exp COMMA Args    {$$=mknode(ARGS,$1,$3,NULL,yylineno);}
       | Exp               {$$=mknode(ARGS,$1,NULL,NULL,yylineno);}
       ;
       
%%

int main(int argc, char *argv[]){
	yyin=fopen(argv[1],"r");
	if (!yyin) return 1;
	yylineno=1;
	yyparse();
	return 0;
	}

#include<stdarg.h>
void yyerror(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "Grammar Error at Line %d Column %d: ", yylloc.first_line,yylloc.first_column);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, ".\n");
}	
