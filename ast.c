#include "define.h"

struct node *mknode(int nodeKind, struct node *first, struct node *second,
                    struct node *third, int pos) {
  struct node *T = (struct node *)malloc(sizeof(struct node));
  T->nodeKind = nodeKind;
  if (first) first->parent = T;
  if (second) second->parent = T;
  if (third) third->parent = T;
  T->ptr[0] = first;
  T->ptr[1] = second;
  T->ptr[2] = third;
  T->pos = pos;
  return T;
}

void display(struct node *T, int indent) {
  if (T) {
    switch (T->nodeKind) {
      case _BREAK:
        printf("%*cBREAK\n", indent, ' ');
        break;
      case _CONTINUE:
        printf("%*cCONTINUE\n", indent, ' ');
        break;
      case EXT_DEF_LIST:
        printf("%*c外部定义列表: \n", indent, ' ');
        strcpy(T->scope, "Global");
        display(T->ptr[0], indent);  //显示外部定义列表的第一个
        display(T->ptr[1], indent);  //显示外部定义列表的其他个
        break;
      case EXT_DEF_VAR:
        printf("%*c外部变量定义: \n", indent, ' ');
        strcpy(T->scope, T->parent->scope);
        display(T->ptr[0], indent + 3);  //显示外部(全局)变量类型
        printf("%*c变量名：\n", indent + 3, ' ');
        display(T->ptr[1], indent + 6);  //显示全局变量列表
        break;
      case EXT_DEF_FUNC:
        printf("%*c外部函数定义：\n", indent, ' ');
        strcpy(T->scope, T->ptr[1]->type_id);
        display(T->ptr[0], indent + 3);  //显示函数返回类型
        display(T->ptr[1], indent + 3);  //显示函数名和参数
        display(T->ptr[2], indent + 3);  //显示函数体
        break;
      // case 外部结构体声明
      case EXT_VARDEC_LIST:
        strcpy(T->scope, "Global");
        display(T->ptr[0], indent);  //依次显示全局变量名
        display(T->ptr[1], indent);
        break;
      case TYPE:
        strcpy(T->scope, T->parent->scope);
        // printf("**** %s\n", T->scope);
        printf("%*c类型：%s\n", indent, ' ', T->type_id);  //显示基本类型
        break;
      //结构体定义还未实现
      case ID:
        strcpy(T->scope, T->parent->scope);
        // printf("**** %s\n", T->scope);
        printf("%*cID：%s\n", indent, ' ', T->type_id);  //显示变量名
        break;
      case ARRAY:
        strcpy(T->scope, T->parent->scope);
        T->width = T->size * (T->type_id == 'char'?1:4);
        printf("%*c%s[] %d\n", indent, ' ', T->type_id, T->size);
        break;
      case ARRAY_CALL:
        strcpy(T->scope, T->parent->scope);
        printf("%*c%s[]\n", indent, ' ', T->type_id);
        display(T->ptr[0], indent + 3);
        break;
      case FUNC_DEC:
        strcpy(T->scope, "Global");
        printf("%*c函数名：%s\n", indent, ' ', T->type_id);
        if (T->ptr[0]) {
          printf("%*c函数形参：\n", indent, ' ');
          display(T->ptr[0], indent + 3);  //显示函数参数列表
        } else
          printf("%*c无参函数\n", indent + 3, ' ');
        break;
      case FUNC_PARAM_LIST:
        strcpy(T->scope, T->parent->scope);
        display(T->ptr[0], indent);  //依次显示参数类型和名称
        display(T->ptr[1], indent);
        break;
      case FUNC_PARAM_DEC:
        strcpy(T->scope, T->parent->scope);
        printf("%*c类型：%s，参数名：%s\n", indent, ' ', T->ptr[0]->type_id,
               T->ptr[1]->type_id);
        break;
      case COMP_STM:
        strcpy(T->scope, T->parent->scope);
        printf("%*c复合语句： \n", indent, ' ');
        printf("%*c复合语句变量定义： \n", indent + 3, ' ');
        display(T->ptr[0], indent + 6);  //显示变量定义部分
        printf("%*c复合语句语句部分： \n", indent + 3, ' ');
        display(T->ptr[1], indent + 6);  //显示语句部分
        printf("%*c复合语句结束\n", indent, ' ');
        break;
      case COMPSTM_LIST:
        strcpy(T->scope, T->parent->scope);
        display(T->ptr[0], indent);  //显示第一条语句
        display(T->ptr[1], indent);  //显示剩下语句
        break;
      case COMPSTM_EXP:
        strcpy(T->scope, T->parent->scope);
        printf("%*c表达式语句：\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        break;
      case RETURN:
        strcpy(T->scope, T->parent->scope);
        printf("%*c返回语句：\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        break;
      case IF_THEN:
        strcpy(T->scope, "IF of ");
        strcat(T->scope, T->parent->scope);
        printf("%*cIF_THEN条件语句：\n", indent, ' ');
        printf("%*cIF条件：\n", indent + 3, ' ');
        display(T->ptr[0], indent + 6);  //显示condition
        printf("%*cTHEN子句：(%d)\n", indent + 3, ' ', T->pos);
        display(T->ptr[1], indent + 6);  //显示if_body
        printf("%*cIF_THEN条件语句结束\n", indent, ' ');
        break;
      case IF_THEN_ELSE:
        strcpy(T->scope, "IF_ELSE of ");
        strcat(T->scope, T->parent->scope);
        printf("%*cIF_THEN_ELSE条件语句：\n", indent, ' ');
        printf("%*cIF条件：\n", indent + 3, ' ');
        display(T->ptr[0], indent + 6);  //显示condition
        printf("%*cTHEN子句：(%d)\n", indent + 3, ' ', T->pos);
        display(T->ptr[1], indent + 6);  //显示if_body
        printf("%*cELSE子句：(%d)\n", indent + 3, ' ', T->pos);
        display(T->ptr[2], indent + 6);  //显示else_body
        printf("%*cIF_THEN_ELSE条件语句结束\n", indent, ' ');
        break;
      case WHILE:
        strcpy(T->scope, "WHILE of ");
        strcat(T->scope, T->parent->scope);
        printf("%*cWHILE循环语句：\n", indent, ' ');
        printf("%*cWHILE条件：\n", indent + 3, ' ');
        display(T->ptr[0], indent + 6);  //显示condition
        printf("%*cWHILE循环体：\n", indent + 3, ' ');
        display(T->ptr[1], indent + 6);  //显示while_body
        printf("%*cWHILE循环语句结束\n", indent, ' ');
        break;
      case FOR:
        strcpy(T->scope, "FOR of ");
        strcat(T->scope, T->parent->scope);
        printf("%*cFOR循环语句：\n", indent, ' ');
        printf("%*cFOR条件：\n", indent + 3, ' ');
        display(T->ptr[0], indent + 6);  //显示condition
        printf("%*cFOR循环体：\n", indent + 3, ' ');
        display(T->ptr[1], indent + 6);  //显示for_body
        printf("%*cFOR循环语句结束：\n", indent, ' ');
        break;
      case FOR_CONDITION:
        strcpy(T->scope, T->parent->scope);
        printf("%*cFOR语句条件1：\n", indent, ' ');
        display(T->ptr[0], indent + 3);  //显示for循环第一个表达式——初始化
        printf("%*cFOR语句条件2：\n", indent, ' ');
        display(T->ptr[1], indent + 3);  //显示for循环第二个表达式
        printf("%*cFOR语句条件3：\n", indent, ' ');
        display(T->ptr[2], indent + 3);  //显示for循环第三个表达式
        break;
      case EXP_FOR1_LIST:
        strcpy(T->scope, T->parent->scope);
      case EXP_FOR3_LIST:
        strcpy(T->scope, T->parent->scope);
        display(T->ptr[0], indent);  //显示for循环第一个表达式
        display(T->ptr[1], indent);  //显示for循环第二个表达式
        break;
      case DEF_LIST:
        strcpy(T->scope, T->parent->scope);
        printf("%*c局部变量定义列表: \n", indent, ' ');
        display(T->ptr[0], indent + 3);  //显示局部变量定义列表中的第一个
        display(T->ptr[1], indent + 3);  //显示其它局部变量定义
        break;
      case VAR_DEF:
        strcpy(T->scope, T->parent->scope);
        printf("%*c局部变量定义: \n", indent, ' ');
        display(T->ptr[0], indent + 3);  //显示局部变量类型
        printf("%*c变量名：\n", indent + 3, ' ');
        display(T->ptr[1], indent + 6);  //显示其他局部变量
        break;
      case VARDEC_LIST:
        strcpy(T->scope, T->parent->scope);
        while (T) {
          strcpy(T->scope, T->parent->scope);
          display(T->ptr[0], indent);
          T = T->ptr[1];
        }
        break;
      case ASSIGNOP:
        strcpy(T->scope, T->parent->scope);
        printf("%*c=\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case ADD_ASSIGNOP:
        strcpy(T->scope, T->parent->scope);
        printf("%*c+=\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case MINUS_ASSIGNOP:
        strcpy(T->scope, T->parent->scope);
        printf("%*c-=\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case STAR_ASSIGNOP:
        strcpy(T->scope, T->parent->scope);
        printf("%*c*=\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case DIV_ASSIGNOP:
        strcpy(T->scope, T->parent->scope);
        printf("%*c/=\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case AND:
        strcpy(T->scope, T->parent->scope);
        printf("%*c&&\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case OR:
        strcpy(T->scope, T->parent->scope);
        printf("%*c||\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case RELOP:
        strcpy(T->scope, T->parent->scope);
        printf("%*c%s\n", indent, ' ', T->type_id);
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case PLUS:
        strcpy(T->scope, T->parent->scope);
        printf("%*c+\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case MINUS:
        strcpy(T->scope, T->parent->scope);
        printf("%*c-\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case STAR:
        strcpy(T->scope, T->parent->scope);
        printf("%*c*\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case DIV:
        strcpy(T->scope, T->parent->scope);
        printf("%*c/\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        display(T->ptr[1], indent + 3);
        break;
      case UMINUS:
        strcpy(T->scope, T->parent->scope);
        printf("%*c-\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        break;
      case NOT:
        strcpy(T->scope, T->parent->scope);
        printf("%*c!\n", indent, ' ');
        display(T->ptr[0], indent + 3);
        break;
      case SELFADD:
        strcpy(T->scope, T->parent->scope);
        display(T->ptr[0], indent);
        printf("%*c++\n", indent + 3, ' ');
        display(T->ptr[1], indent + 6);
        break;
      case SELFDEC:
        strcpy(T->scope, T->parent->scope);
        display(T->ptr[0], indent);
        printf("%*c--\n", indent + 3, ' ');
        display(T->ptr[1], indent + 6);
        break;
      case FUNC_CALL:
        strcpy(T->scope, T->parent->scope);
        printf("%*c函数调用：\n", indent, ' ');
        printf("%*c函数名：%s\n", indent + 3, ' ', T->type_id);
        if (T->ptr[0]) {
          printf("%*c函数参数：\n", indent + 3);
          display(T->ptr[0], indent + 6);
        }
        break;
      case INT:
        strcpy(T->scope, T->parent->scope);
        printf("%*cINT：%d\n", indent, ' ', T->type_int);
        break;
      case FLOAT:
        strcpy(T->scope, T->parent->scope);
        printf("%*cFLOAT：%f\n", indent, ' ', T->type_float);
        break;
      case CHAR:
        strcpy(T->scope, T->parent->scope);
        printf("%*cCHAR：'%c'\n", indent, ' ', T->type_char);
        break;
      case ARGS:
        while (T) {
          display(T->ptr[0], indent);
          T = T->ptr[1];
        }
        break;
    }
  }
}

//用于显示符号表的语法树，确定作用域并显示
// paraCnt用于防止函数带有多个参数时paralist嵌套定义带来错误
int paraCnt = 0;
void udisplay(struct node *T, int indent) {
  if (T) {
    switch (T->nodeKind) {
      case _BREAK:
        strcpy(T->scope, T->parent->scope);
        break;
      case _CONTINUE:
        strcpy(T->scope, T->parent->scope);
        break;
      case EXT_DEF_LIST:
        strcpy(T->scope, "Global");
        udisplay(T->ptr[0], indent + 3);  //显示外部定义列表的第一个
        udisplay(T->ptr[1], indent + 3);  //显示外部定义列表的其他个
        break;
      case EXT_DEF_VAR:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);  //显示外部(全局)变量类型
        udisplay(T->ptr[1], indent + 3);  //显示全局变量列表
        break;
      case EXT_DEF_FUNC:
        //函数名作为作用域
        strcpy(T->scope, T->ptr[1]->type_id);
        udisplay(T->ptr[0], indent + 3);  //显示函数返回类型
        udisplay(T->ptr[1], indent + 3);  //显示函数名和参数
        udisplay(T->ptr[2], indent + 3);  //显示函数体
        break;
      // case 外部结构体声明
      case EXT_VARDEC_LIST:
        strcpy(T->scope, "Global");
        udisplay(T->ptr[0], indent);  //依次显示全局变量名
        udisplay(T->ptr[1], indent);
        break;
      case TYPE:
        strcpy(T->scope, T->parent->scope);
        // printf("**** %s\n", T->scope);

        break;
      //结构体定义还未实现
      case ID:
        strcpy(T->scope, T->parent->scope);
        // printf("**** %s\n", T->scope);

        break;
      case ARRAY:
        strcpy(T->scope, T->parent->scope);
        T->width = T->size * (T->type_id == 'char'?1:4);
        break;
      case ARRAY_CALL:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        break;
      case FUNC_DEC:
        strcpy(T->scope, T->type_id);
        // printf("%s\n", T->type_id);
        if (T->ptr[0]) {
          udisplay(T->ptr[0], indent + 3);  //显示函数参数列表
        }
        break;
      case FUNC_PARAM_LIST:
        //防止多参数时出现多个paraof
        if (paraCnt == 0) {
          strcpy(T->scope, "Para of ");
          paraCnt++;
        }
        strcat(T->scope, T->parent->scope);
        // printf("******* %s %s\n", T->scope,T->type_id);
        udisplay(T->ptr[0], indent);  //依次显示参数类型和名称
        udisplay(T->ptr[1], indent);
        paraCnt--;
        break;
      case FUNC_PARAM_DEC:
        strcpy(T->scope, T->parent->scope);

        break;
      case COMP_STM:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);  //显示变量定义部分
        udisplay(T->ptr[1], indent + 3);  //显示语句部分
        break;
      case COMPSTM_LIST:
        strcpy(T->scope, T->parent->scope);
        udisplay(T->ptr[0], indent);  //显示第一条语句
        udisplay(T->ptr[1], indent);  //显示剩下语句
        break;
      case COMPSTM_EXP:
        strcpy(T->scope, T->parent->scope);
        // printf("%*c表达式语句: ", indent, ' ');
        udisplay(T->ptr[0], indent);
        break;
      case RETURN:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        break;
      case IF_THEN:
        strcpy(T->scope, "IF of ");
        strcat(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);  //显示condition
        udisplay(T->ptr[1], indent + 3);  //显示if_body
        break;
      case IF_THEN_ELSE:
        strcpy(T->scope, "IF_ELSE of ");
        strcat(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);  //显示condition
        // printf("%*cstmt_ifbody:", indent+3, ' ');
        udisplay(T->ptr[1], indent + 3);  //显示if_body
        // printf("%*cstmt_elsebody:", indent+3, ' ');
        udisplay(T->ptr[2], indent + 3);  //显示else_body
        break;
      case WHILE:
        strcpy(T->scope, "WHILE of ");
        strcat(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);  //显示condition
        udisplay(T->ptr[1], indent + 3);  //显示while_body
        break;
      case FOR:
        strcpy(T->scope, "FOR of ");
        strcat(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);  //显示condition
        udisplay(T->ptr[1], indent + 3);  //显示for_body
        break;
      case FOR_CONDITION:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);  //显示for循环第一个表达式——初始化
        udisplay(T->ptr[1], indent + 3);  //显示for循环第二个表达式
        udisplay(T->ptr[2], indent + 3);  //显示for循环第三个表达式
        break;
      case EXP_FOR1_LIST:
        strcpy(T->scope, T->parent->scope);
        udisplay(T->ptr[0], indent);  //显示for循环第一个表达式——初始化
        udisplay(T->ptr[1], indent);  //显示for循环第二个表达式
        break;
      case EXP_FOR3_LIST:
        strcpy(T->scope, T->parent->scope);
        udisplay(T->ptr[0], indent);  //显示for循环第一个表达式——初始化
        udisplay(T->ptr[1], indent);  //显示for循环第二个表达式
        break;
      case DEF_LIST:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);  //显示局部变量定义列表中的第一个
        udisplay(T->ptr[1], indent + 3);  //显示其它局部变量定义
        break;
      case VAR_DEF:
        strcpy(T->scope, T->parent->scope);
        udisplay(T->ptr[0], indent);  //显示局部变量类型
        udisplay(T->ptr[1], indent);  //显示其他局部变量
        break;
      case VARDEC_LIST:
        strcpy(T->scope, T->parent->scope);
        while (T) {
          strcpy(T->scope, T->parent->scope);
          udisplay(T->ptr[0], indent);
          T = T->ptr[1];
        }
        break;
      case ASSIGNOP:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case ADD_ASSIGNOP:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case MINUS_ASSIGNOP:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case STAR_ASSIGNOP:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case DIV_ASSIGNOP:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case AND:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case OR:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case RELOP:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case PLUS:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case MINUS:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case STAR:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case DIV:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        udisplay(T->ptr[1], indent + 3);
        break;
      case UMINUS:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        break;
      case NOT:
        strcpy(T->scope, T->parent->scope);

        udisplay(T->ptr[0], indent + 3);
        break;
      case SELFADD:
        strcpy(T->scope, T->parent->scope);
        udisplay(T->ptr[0], indent);

        udisplay(T->ptr[1], indent + 6);
        break;
      case SELFDEC:
        strcpy(T->scope, T->parent->scope);
        udisplay(T->ptr[0], indent);

        udisplay(T->ptr[1], indent + 6);
        break;
      case FUNC_CALL:
        strcpy(T->scope, T->parent->scope);

        if (T->ptr[0]) {
          udisplay(T->ptr[0], indent + 6);
        }
        break;
      case INT:
        strcpy(T->scope, T->parent->scope);

        break;
      case FLOAT:
        strcpy(T->scope, T->parent->scope);

        break;
      case CHAR:
        strcpy(T->scope, T->parent->scope);

        break;
      case ARGS:
        while (T) {
          udisplay(T->ptr[0], indent);
          T = T->ptr[1];
        }
        break;
    }
  }
}