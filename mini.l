%{
  #include <stdio.h>
  #include <string.h>
  #include <stdlib.h>
  #include "tokens.h"

  int linea = 1;


%}

%x COMENTARIO
%x COMENTARIO_LINEA
%x STRING
%x PATH


IDENTIFICADOR     [[:alpha:]_][[:alpha:]_0-9]*
TOKEN_UN_CARACTER [A-Z]

ENTERO_DEC        [0-9]+
ENTERO_OCT        #[0-7]+
ENTERO_HEX        #[Hh][0-9A-Fa-f]+
ENTERO            {ENTERO_DEC}|{ENTERO_HEX}|{ENTERO_OCT}
REAL_DEC          [0-9]*\.{ENTERO_DEC}
REAL_HEX          #[Hh][0-9A-Fa-f]*\.[0-9A-Fa-f]+
REAL_OCT          #[0-7]*\.[0-7]+
REAL              {REAL_DEC}|{REAL_HEX}|{REAL_OCT}
REAL_EXP          ({ENTERO}|{REAL})exp[+-]?{ENTERO}

CARACTER_ESCAPADO          '#[\'\#abfnrtv]'
CARACTER                   '[^\#'\n]'|{CARACTER_ESCAPADO}|'\#[1]{1}[0-7]{2}'|'\#[Hh][0-7]{1}[0-9A-Fa-f]'
FICHERO                    [[:alpha:]_0-9]+\.[:alpha:]+
DIRECTORIO                 [\/?[:alpha:]_][[:alpha:]_0-9]*
RUTA                       \|((\/)?([^\n|\/])+\/)*([^\n\|\/]+\.[^\n|\/]+)\|

DELIMITADOR          [\(\)\{\}\;\:\,]
OP_ARITMETICO        [\+\-\*\/\%]
OP_BITS              [\&\|\@\~]
OP_RELACIONAL        [\<\>]
OP_LOGICO            [\!]
OP_ASIGNACION        [\=]
OP_MEMORIA           [\.\[\]\{\}\^\\]

ESCAPES	             \ |\n|\t|\v|\r
COM		               "##"[^\n]*

 

%%


\n                linea++;

(?i:array)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(ARRAY);}
(?i:cabecera)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(CABECERA);}
(?i:cada)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(CADA);}
(?i:cadena)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(CADENA);}
(?i:caracter)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(CARACTER);}
(?i:conjunto)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(CONJUNTO);}
(?i:constantes)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(CONSTANTES);}
(?i:continuar)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(CONTINUAR);}
(?i:de)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(DE);}
(?i:defecto)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(DEFECTO);}
(?i:devolver)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(DEVOLVER);}
(?i:ejecuta)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(EJECUTA);}
(?i:encambio)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(ENCAMBIO);}
(?i:entero)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(ENTERO);}
(?i:es)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(ES);}
(?i:escape)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(ESCAPE);}
(?i:estructura)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(ESTRUCTURA);}
(?i:etiqueta)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(ETIQUETA);}
(?i:excepcion)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(EXCEPCION);}
(?i:fichero)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(FICHERO);}
(?i:fin)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(FIN);}
(?i:funcion)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(FUNCION);}
(?i:hacer)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(HACER);}
(?i:hash)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(HASH);}
(?i:lanza)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(LANZA);}
(?i:mientras)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(MIENTRAS);}
(?i:nada)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(NADA);}
(?i:otra)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(OTRA);}
(?i:para)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(PARA);}
(?i:principio)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(PRINCIPIO);}
(?i:programa)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(PROGRAMA);}
(?i:real)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(REAL);}
(?i:ref)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(REF);}
(?i:saltar)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(SALTAR);}
(?i:si)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(SI);}
(?i:sino)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(SINO);}
(?i:tipos)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(TIPOS);}
(?i:union)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(UNION);}
(?i:variables)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(VARIABLES);}
(?i:tamano)        {printf("linea %d, palabra reservada: %s\n", linea, yytext);
                  return(TAMANO);}


{IDENTIFICADOR}   {printf("linea %d, identificador: %s\n", linea, yytext);
                  return(IDENTIFICADOR);}

{ENTERO}      {printf("linea %d, ctc entera: %s\n", linea, yytext);
                                            return(CTC_ENTERA);}

{REAL}|{REAL_EXP}      {printf("linea %d, ctc real: %s\n", linea, yytext);
                                            return(CTC_REAL);}
{CARACTER}             {printf("linea %d, ctc caracter: %s\n", linea, yytext);
                        return(CTC_CARACTER);}
{RUTA}                  {printf("linea %d, ruta: %s\n", linea, yytext);
                        return(RUTA);}

{FICHERO}               {printf("fichero %s\n", yytext);}                                     


\'                      {printf("linea %d, cadena: %s", linea, yytext);
                        BEGIN(STRING);}
<STRING>\n          {linea++;
                        printf("%s", yytext);}
<STRING>\#[\'\#abfnrtv]              printf("%s", yytext);
<STRING>\#[1]{1}[0-7]{2}             printf("%s", yytext);
<STRING>\#[Hh][0-7]{1}[0-9A-Fa-f]    printf("%s", yytext);
<STRING>"\'"            {BEGIN(INITIAL);
                        printf("%s\n", yytext);
                        return(CTC_CADENA);}



{DELIMITADOR}           {printf("linea %d, delimitador: %s\n", linea, yytext);}

{OP_ARITMETICO}        {printf("linea %d, operador: %s\n", linea, yytext);
                        return(yytext[0]);}
{OP_BITS}              {printf("linea %d, operador: %s\n", linea, yytext);
                        return(yytext[0]);}
{OP_RELACIONAL}        {printf("linea %d, operador: %s\n", linea, yytext);
                        return(yytext[0]);}
{OP_LOGICO}            {printf("linea %d, operador: %s\n", linea, yytext);
                        return(yytext[0]);}
{OP_ASIGNACION}        {printf("linea %d, operador: %s\n", linea, yytext);
                        return(yytext[0]);}
{OP_MEMORIA}           {printf("linea %d, operador: %s\n", linea, yytext);
                        return(yytext[0]);}

"**"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(POTENCIA);}
"<-"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(FLECHA_IZDA);} 
"->"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(FLECHA_DCHA);} 
"&&"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(AND);} 
"||"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(OR);} 
"=<"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(LE);} 
"=>"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(GE);} 
"=="                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(EQ);} 
"!="                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(NEQ);} 
"=+"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(SUMA_ASIG);}
"=-"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(RESTA_ASIG);} 
"=*"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(MULT_ASIG);} 
"=/"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(DIV_ASIG);} 
"=%"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(MOD_ASIG);} 
"=**"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(POT_ASIG);} 
"=<-"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(FI_ASIG);} 
"=->"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(FD_ASIG);} 
"=&"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(AND_ASIG);} 
"=@"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(XOR_ASIG);}
"=|"                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(OR_ASIG);}   
"^."                   {printf("linea %d, operador: %s\n", linea, yytext);
                        return(INDIRECCION);}                               

"#-"              BEGIN(COMENTARIO);
<COMENTARIO>.|\n ;
<COMENTARIO>"-#"  BEGIN(INITIAL);



{COM}
{ESCAPES}             

.                 {printf("linea %d, error encontrado: %s\n", linea, yytext);}


%%

int yywrap() {
  return(1);
  }

int main(int argc, char *argv[]) {

  if (argc < 2) {
    printf("Uso: ./mini NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    while (yylex());
    }

}
