%{
#define LIST               strcat(buf, yytext)
#define token(t)           {LIST; if (Opt_T) printf("<%s>\n", #t);}
#define tokenChar(t)       {LIST; if (Opt_T) printf("<%c>\n", (t));}
#define tokenInteger(t, i) {LIST; if (Opt_T) printf("<%s: %d>\n", #t, (i));}
#define tokenString(t, s)  {LIST; if (Opt_T) printf("<%s: %s>\n", #t, (s));}
#define MAX_LINE_LENG      256

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
<str>.*\"  {pch=strtok(yytext,"\"");tokenString(string,pch);strcat(buf, "\"");BEGIN 0;}

   /*  Delimiter  */

","  {tokenChar(',');}
";"  {tokenChar(';');}
":"  {tokenChar(':');}
"("  {tokenChar('(');}
")"  {tokenChar(')');}
"["  {tokenChar('[');}
"]"  {tokenChar(']');}


   /*  Operator  */

"+"  {token(+);}
"-"  {token(-);}
"*"  {token(*);}
"/"  {token(/);}
"mod"  {token(mod);}
":="  {token(:=);}
"<"  {token(<);}
"<="  {token(<=);}
"<>"  {token(<>);}
">="  {token(>=);}
">"  {token(>);}
"="  {token(=);}
"and"  {token(and);}
"or"  {token(or);}
"not"  {token(not);}

   /*  Keywords  */

"array"    {token(KWarray);}
"begin"    {token(KWbegin);}
"boolean"  {token(KWboolean);}
"def"      {token(KWdef);}
"do"       {token(KWdo);}
"else"     {token(KWelse);}
"end"      {token(KWend);}
"false"    {token(KWfalse);}
"for"      {token(KWfor);}
"integer"  {token(KWinteger);}
"if"       {token(KWif);}
"of"       {token(KWof);}
"print"    {token(KWprint);}
"read"     {token(KWread);}
"real"     {token(KWreal);}
"string"   {token(KWstring);}
"then"     {token(KWthen);}
"to"       {token(KWto);}
"true"     {token(KWtrue);}
"return"   {token(KWreturn);}
"var"      {token(KWvar);}
"while"    {token(KWwhile);}

   /*  Identifier  */

{letter}({letter}|{digit})*  {tokenString(id,yytext);}

   /*  Scientific Notations  */

-?{digit}+(\.{digit}+)?[Ee][-+]?{digit}+ {tokenString(scientific,yytext);}

   /*  Numbers  */

-?0|[1-9][0-9]*  {tokenString(integer,yytext);}
0[1-7][0-7]*   {tokenString(oct_integer,yytext);}
-?{digit}+\.{digit}+  {tokenString(float,yytext);}



%%
int main( int argc, char **argv )
{
  if( argc != 2 ) {
    fprintf( stderr, "Usage: ./scanner [filename]\n" );
    exit(0);
  }
	
  FILE *fp = fopen( argv[1], "r" );
  if( fp == NULL ) {
    fprintf( stderr, "Open file error\n" );
    exit(-1);
  }
	
  yyin = fp;

  yylex();

  exit(0);
}
