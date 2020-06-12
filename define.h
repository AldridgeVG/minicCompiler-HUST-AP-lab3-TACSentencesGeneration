#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "parser.tab.h"

#define DX 3*sizeof(int)          //活动记录控制信息需要的单元数，这个根据实际系统调整

#define MAXLENGTH   1000    //定义符号表的大小
//结点类别相当于有非终结符 和 终结符(.l文件词法分析Return的都是)
enum node_kind{_BREAK,_CONTINUE,EXT_DEF_LIST,EXT_DEF_VAR,EXT_DEF_FUNC,EXT_VARDEC_LIST,FUNC_DEC,FUNC_PARAM_LIST,FUNC_PARAM_DEC,COMP_STM,COMPSTM_LIST,COMPSTM_EXP,IF_THEN,IF_THEN_ELSE,DEF_LIST,VAR_DEF,VARDEC_LIST,FUNC_CALL,ARRAY_CALL,ARGS,FOR_CONDITION,EXP_FOR1_LIST,EXP_FOR3_LIST};

struct node{
    enum node_kind nodeKind;    //结点类型
    union{
        int type_int;
	    float type_float;
        char type_char;
	    char type_id[32];
    };
    struct node *ptr[3];   
    struct node *parent;//父节点   
    int pos;        //记录语法单位所在的位置行号
    int size;         //记录数组的大小
    int index;      //记录数组的下标
    char scope[50]; //所属作用域
    char Etrue[15],Efalse[15];       //对布尔表达式的翻译时，真假转移目标的标号
    char Snext[15];               //结点对应语句S执行后的下一条语句位置标号
    struct codenode *code;          //该结点中间代码链表头指针
    int level;      //层号
    int place;      //结点对应的变量/运算结果符号表的位置序号
    int type;       //结点对应的值的类型
    int num;        //记录子结点个数（包括自己）
    int offset;     //偏移量
    int width;                     //占数据字节数
    int return_num;
    int break_num;
};

struct opn{
    int kind;                  //标识操作的类型
    int type;                  //标识操作数的类型
    union {
        int     const_int;      //整常数值，立即数
        float   const_float;    //浮点常数值，立即数
        char    const_char;    //字符常数值，立即数
        char    id[33];        //变量或临时变量的别名或标号字符串
        };
    int level;                 //变量的层号，0表示是全局变量，数据保存在静态数据区
    int offset;                 //变量单元偏移量，或函数在符号表的定义位置序号，目标代码生成时用
};

//三地址TAC代码结点,采用单链表存放中间语言代码
struct codenode {   
        int  op;
        struct opn opn1,opn2,result;
        struct codenode  *next,*prior;
    };

//符号表的行
struct symbol{  
    char name[33];     //变量或函数名
    int level;              //层号，外部变量名或函数名层号为0，形参名为1，每到1个复合语句层号加1，退出减1
    int type;           //变量类型 或 函数返回值类型
    int  paramnum;    //形参个数     
    char alias[10];      //别名，为解决嵌套层次使用，使得每一个数据名称唯一
    char flag;          //符号标记，数组：‘A'  函数：'F'  变量：'V'   参数：'P'  临时变量：'T'
    int offset;      //外部变量和局部变量在其静态数据区或活动记录中的偏移量，
//或记录函数活动记录大小，目标代码生成时使用
};

//符号表（顺序栈），index初值为0
struct symboltable{
    struct symbol symbols[MAXLENGTH];
    int index;
} symbolTable;


struct symbol_scope_begin {  
    int TX[30];
    int top;
} symbol_scope_TX;

struct node *mknode(int kind,struct node *first,struct node *second, struct node *third,int pos );
char *strcats(char *s1,char *s2);
char *createAlias();
char *createTemp();
void semantic_error(int line,char *msg1,char *msg2);
void prn_symbol();
int searchSymbolTable(char *name);
int fillSymbolTable(char *name,char *alias,int level,int type,char flag,int offset);
int fill_Temp(char *name,int level,int type,char flag,int offset);
void ext_var_list(struct node *T); //未完
int  match_param(int i,struct node *T);
void objectCode(struct codenode *head);
void semantic_Analysis(struct node *T);//未完
void Exp(struct node *T);
void boolExp(struct node *T);
void semantic_Analysis0(struct node *T);