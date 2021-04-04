%{
  #include <stdio.h>
  #include <string.h>
  #include <stdlib.h>
  #include "tokens.h"

  int linea = 1;


%}

SP             [ \t]
SN             [ \n]
IDENTIFICADOR [[:alpha:]_][[:alpha:]_0-9]*
TOKEN_UN_CARACTER [A-Z]

ENTERO_DEC [0-9]+
ENTERO_OCT #[0-7]+
ENTERO_HEX #[Hh][0-9A-Fa-f]+
REAL_PARTE_ENTERA [{ENTEROS_DEC}{ENTEROS_HEX}{ENTEROS_OCT}][.][{ENTEROS_DEC}{ENTEROS_HEX}{ENTEROS_OCT}]


%%

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

{IDENTIFICADOR}   {printf("linea %d\n, identificador: %s\n", linea, yytext);}
{ENTERO_DEC}      {printf("entero %s\n", yytext);}
{ENTERO_OCT}      {printf("octal %s\n", yytext);}
{ENTERO_HEX}      {printf("hexadecimal %s\n", yytext);}
{REAL_PARTE_ENTERA} {printf("real %s\n", yytext);}

'**'             return(POTENCIA);
'=+'             return (SUMA_ASIG);
'=-'             return (RESTA_ASIG);

\n                linea++;


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
