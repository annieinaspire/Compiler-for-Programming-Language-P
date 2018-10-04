%{
#define LIST               strcat(buf, yytext)
#define token(t)           {LIST; if (Opt_T) printf("<%s>\n", #t);}
#define tokenChar(t)       {LIST; if (Opt_T) printf("<%c>\n", (t));}
#define tokenInteger(t, i) {LIST; if (Opt_T) printf("<%s: %d>\n", #t, (i));}
#define tokenString(t, s)  {LIST; if (Opt_T) printf("<%s: %s>\n", #t, (s));}
#define MAX_LINE_LENG      256

#include "y.tab.h"

int Opt_S = 1;
int Opt_T = 1;
int linenum = 1;
char buf[MAX_LINE_LENG];
char * pch;
%}

%s com1 com2 com3 str

digit  [0-9]
letter [a-zA-Z_]


%%
  /*  cppComments  */

"//" {LIST;BEGIN com1;}
<com1>.* {LIST;BEGIN com1;}
<com1>\n {
          LIST;
          if (Opt_S)
            printf("%d: %s", linenum, buf);
          linenum++;
          buf[0] = '\0';
          BEGIN 0;
        }

  /*  cComments  */

"/*" {LIST;BEGIN com2;}
<com2>.*\*\/ {
            LIST;
            BEGIN 0;
           }
<com2>.* {
            LIST;
            BEGIN com2;
           }
<com2>\n {
          LIST;
          if (Opt_S)
            printf("%d: %s", linenum, buf);
          linenum++;
          buf[0] = '\0';
          BEGIN com2;
        }

  /*  Pseudo comments  */

"//&S-"  {LIST;Opt_S = 0;BEGIN com3;}
"//&S+"  {LIST;Opt_S = 1;BEGIN com3;}
"//&T-"  {LIST;Opt_T = 0;BEGIN com3;}
"//&T+"  {LIST;Opt_T = 1;BEGIN com3;}
<com3>.* {LIST;BEGIN com3;}
<com3>\n {
          LIST;
          if (Opt_S)
            printf("%d: %s", linenum, buf);
          linenum++;
          buf[0] = '\0';
          BEGIN 0;
        }

  /*  newline  */

\n      {
          LIST;
          if (Opt_S)
            printf("%d: %s", linenum, buf);
          linenum++;
          buf[0] = '\0';
        }

   /* space and tab  */

[ \t]   {LIST;}

   /* StringType = strtok(StringType,StringType); StringType: "XXX";  */
   /* \" : obtain " ; \* : obtain * ; \/ : obtain / ; */
   /*  string  */

"\""  {LIST;BEGIN str;}
<str>.*\"  {pch=strtok(yytext,"\"");tokenString(string,pch);strcat(buf, "\"");BEGIN 0;return str_cons;}

   /*  Delimiter  */

","  {tokenChar(',');return COMMA;}
";"  {tokenChar(';');return SEMICOLON;}
":"  {tokenChar(':');return COLON;}
"("  {tokenChar('(');return LEFTS;}
")"  {tokenChar(')');return RIGHTS;}
"["  {tokenChar('[');return LEFTM;}
"]"  {tokenChar(']');return RIGHTM;}


   /*  Operator  */

"+"  {token(+);return ADD;}
"-"  {token(-);return SUB;}
"*"  {token(*);return MUL;}
"/"  {token(/);return DIV;}
"mod"  {token(mod);return MOD;}
":="  {token(:=);return ASSIGN;}
"<"  {token(<);return LT;}
"<="  {token(<=);return LE;}
"<>"  {token(<>);return NE;}
">="  {token(>=);return GE;}
">"  {token(>);return GT;}
"="  {token(=);return EQ;}
"and"  {token(and);return AND;}
"or"  {token(or);return OR;}
"not"  {token(not);return NOT;}

   /*  Keywords  */

"array"    {token(KWarray);return KWarray;}
"begin"    {token(KWbegin);return KWbegin;}
"boolean"  {token(KWboolean);return KWboolean;}
"def"      {token(KWdef);return KWdef;}
"do"       {token(KWdo);return KWdo;}
"else"     {token(KWelse);return KWelse;}
"end"      {token(KWend);return KWend;}
"false"    {token(KWfalse);return KWfalse;}
"for"      {token(KWfor);return KWfor;}
"integer"  {token(KWinteger);return KWinteger;}
"if"       {token(KWif);return KWif;}
"of"       {token(KWof);return KWof;}
"print"    {token(KWprint);return KWprint;}
"read"     {token(KWread);return KWread;}
"real"     {token(KWreal);return KWreal;}
"string"   {token(KWstring);return KWstring;}
"then"     {token(KWthen);return KWthen;}
"to"       {token(KWto);return KWto;}
"true"     {token(KWtrue);return KWtrue;}
"return"   {token(KWreturn);return KWreturn;}
"var"      {token(KWvar);return KWvar;}
"while"    {token(KWwhile);return KWwhile;}

   /*  Identifier  */

{letter}({letter}|{digit})*  {tokenString(id,yytext);return ID;}

   /*  Scientific Notations  */

-?{digit}+(\.{digit}+)?[Ee][-+]?{digit}+ {tokenString(scientific,yytext);return SCIEN;}

   /*  Numbers  */

-?0|[1-9][0-9]*  {tokenString(integer,yytext);return int_cons;}
0[1-7][0-7]*   {tokenString(oct_integer,yytext);return oct_cons;}
-?{digit}+\.{digit}+  {tokenString(float,yytext);return real_cons;}



%%

int yywrap(){return 1;}

