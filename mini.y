 
%{

  #include <stdio.h>
  extern FILE *yyin;
  extern int yylex();
  extern int num_linea;

  #define YYDEBUG 1

  int yylex(void);
  int yyerror(char *);

%}

%token AND AND_ASIG ARRAY CABECERA CADA CADENA CARACTER CONJUNTO CONSTANTES CONTINUAR CTC_CADENA
%token CTC_CARACTER CTC_ENTERA CTC_REAL DE DEFECTO DIV_ASIG DEVOLVER EJECUTA ENCAMBIO ENTERO EQ ES
%token ESCAPE ESTRUCTURA ETIQUETA EXCEPCION FD_ASIG FI_ASIG FLECHA_DCHA FLECHA_IZDA FICHERO FIN
%token FUNCION HACER HASH GE IDENTIFICADOR INDIRECCION LANZA LE MIENTRAS MOD_ASIG MULT_ASIG NADA NEQ
%token OR OTRA OR_ASIG PARA POT_ASIG PRINCIPIO PROGRAMA POTENCIA REAL REF RESTA_ASIG RUTA SALTAR SI
%token SINO SUMA_ASIG TAMANO TIPOS UNION VARIABLES XOR_ASIG 


//%nonassoc '-' '~' '!' TAMANO


%%


//Víctor Busto Mejuto,  Andrea García Mato


/************/
/* programa */
/************/

/************************/
/* declaracion de tipos */
/************************/

/*
lista_expresiones: declaraciones_constantes
  | lista_expresiones declaraciones_constantes
;
*/

declaraciones_tipos
  : TIPOS declaracion_tipo FIN                      {printf("	declaraciones_tipos -> declaracion_tipo\n");}
;

declaracion_tipo
  : IDENTIFICADOR ES especificacion_tipo ';'        {printf("	declaracion_tipo -> declaracion_tipo\n");}
;

especificacion_tipo
  : referencia tipo_basico                          {printf("	especificacion_tipo -> especificacion_tipo\n");}

;

referencia
  : 
  | referencia                                      {printf("Referencia\n");}        
;

tipo_basico
  : tipo_escalar                                    {printf(" tipo_escalar\n");}
  | tipo_enumerado                                  {printf("	tipo_enumerado\n");}
  | tipo_estructurado                               {printf("	tipo_estructurado\n");}
;

tipo_escalar
  : ENTERO | REAL | CARACTER | CADENA | FICHERO | EXCEPCION {printf("	tipo_escalar\n");}
;

tipo_enumerado
  : ARRAY DE especificacion_tipo                    {printf("	tipo_enumerado -> ARRAY\n");}
  | HASH DE especificacion_tipo                     {printf("	tipo_enumerado -> HASH\n");}
  | CONJUNTO DE especificacion_tipo                 {printf("	tipo_enumerado -> CONJUNTO\n");}
;

tipo_estructurado
  : ESTRUCTURA PRINCIPIO linea_campo FIN            {printf("	tipo_estructurado -> ESTRUCTURA\n");}
  | UNION PRINCIPIO linea_campo FIN                 {printf("	tipo_estructurado -> UNION\n");}
;

linea_campo
  : linea_campo                                     {printf("	linea_campo\n");}
  | identificadores ES especificacion_tipo ';'      {printf("	linea_campo -> especificacion_tipo\n");}
;

identificadores
  : IDENTIFICADOR                                    {printf("	IDENTIFICADOR\n");}
  | identificadores                                  {printf("	identificadores\n");}
;


/*****************************/
/* declaracion de constantes */

declaraciones_constantes
  : CONSTANTES declaracion_constante FIN                  {printf("	declaraciones_constantes -> CONSTANTES\n");}
;

declaracion_constante
  : IDENTIFICADOR ES tipo_basico '=' constante ';'        {printf("	declaracion_constante -> IDENTIFICADOR\n");}
;

constante  
	: CTC_ENTERA                 	{printf("	constante -> CTC_ENTERA\n");}                              
	| CTC_REAL 						        {printf("	constante -> CTC_REAL\n");}
	| CTC_CADENA 					        {printf("	constante -> CTC_CADENA\n");}
	| CTC_CARACTER 					      {printf("	constante -> CTC_CARACTER\n");}
  | constante_enumerada         {printf(" constante -> constante_enumerada");}
  | constante_estructurada      {printf(" constante -> constante_estructurada");}
;

constante_enumerada
  : '(' ')'                         {printf(" ctc_enumerada -> lista_vacia");}
  | '(' lista_constantes ')'        {printf(" ctc_enumerada -> lista_de_constantes");}
  | '(' tabla_hash ')'              {printf(" ctc_enumerada -> tabla_hash");}
;

lista_constantes
    : constante                       { printf ("  lista_constantes -> constante \n"); }  
    | lista_constantes ',' constante  { printf ("  lista_constantes -> lista_cte',' constante \n");}        
;

constante_estructurada
  : '{'  campo_constante '}' {printf("	constante_estructurada\n");}
;

campo_constante
  : IDENTIFICADOR '=' constante {printf("	IDENTIFICADOR -> constante\n");}
  | campo_constante ',' IDENTIFICADOR '=' constante {printf("	IDENTIFICADOR -> constante\n");}
;

tabla_hash
  : elemento_hash                   {printf ("  tabla_hash -> elemento_hash \n"); }
  | tabla_hash ',' elemento_hash    {printf ("  tabla_hash -> tabla_hash , elemento_hash \n"); }
;


elemento_hash
    : CTC_CADENA FLECHA_DCHA constante        { printf ("  elemento_hash -> CTC_CADENA '->' constante_cadena \n"); }
  ;

/*****************************/


/****************************/
/* declaracion de variables */
/****************************/


/****************************/
/* declaracion de funciones */
/****************************/

/*bloque_instrucciones
   :PRINCIPIO instrucciones FIN { printf(" bloque_instruciones -> bloque_instrucciones"); }
;

instrucciones
  : instruccion { printf(" instrucion -> instruccion"); }
  | instrucciones instruccion { printf(" instrucion -> instrucciones"); }
;
*/

/*****************/
/* instrucciones */
/*****************/

/*

instruccion
  : instruccion_expresion           { printf(" instrucion -> instruccion_expresion"); }
  | instruccion_bifurcacion           { printf(" instrucion -> instruccion_bifurcacion"); }
  | instruccion_bucle           { printf(" instrucion -> instruccion_bucle"); }
  | instruccion_salto           { printf(" instrucion -> instruccion_salto"); }
  | instruccion_destino_salto           { printf(" instrucion -> instruccion_destino_salto"); }
  | instruccion_devolver           { printf(" instrucion -> instruccion_devolver"); }
  | instruccion_vacia           { printf(" instrucion -> instruccion_vacia"); }
  | instruccion_lanzamiento_excepcion           { printf(" instrucion -> instruccion_lanzamiento_excepcion"); }
  | instruccion_captura_excepcion           { printf(" instrucion -> instruccion_lanzamiento_excepcion"); }
;

instruccion_expresion
  : expresion_funcional ';' { printf("instruccion_expresion -> expresion_funcional"); }
  | asignacion ';' { printf("instruccion_expresion -> asignacion"); }   
;

asignacion
  : expresion_indexada operador_asignacion expresion_basica { printf("asignacion -> asignacion"); }
;

operador_asignacion
  : '=' { printf("operador_asignacion -> ="); }
  | SUMA_ASIG { printf("asignacion -> =+"); }
  | RESTA_ASIG { printf("asignacion -> =-"); }
  | MULT_ASIG { printf("asignacion -> =*"); }
  | DIV_ASIG { printf("asignacion -> =/"); }
  | MOD_ASIG { printf("asignacion -> =%"); }
  | POT_ASIG { printf("asignacion -> =**"); }
  | FI_ASIG { printf("asignacion -> =<-"); }
  | FD_ASIG { printf("asignacion -> =->"); }
  | AND_ASIG { printf("asignacion -> =&"); }
  | XOR_ASIG { printf("asignacion -> =@"); }
  | OR_ASIG { printf("asignacion -> =|"); }
;

instruccion_bifurcacion
  : SI '(' expresion ')' accion otros sino_accion FIN { printf("instruccion_bifurcacion -> instruccion_bifurcacion"); }
;

otros
  : 
  | otros otros_casos  { printf("otros -> otros"); }
;


otros_casos
  : SI ENCAMBIO '(' expresion ')' accion  { printf("otros_casos -> otros_casos"); }
;

sino_accion
  :
  | SINO accion { printf("sino_accion -> sino_accion"); }
  ;

accion
  : instruccion { printf("accion -> instruccion"); }
  | bloque_instrucciones { printf("accion -> bloque_instrucciones"); }
;

instruccion_bucle
  : MIENTRAS '(' expresion ')' accion { printf("instruccion_bucle -> while"); }
  | HACER accion MIENTRAS '(' expresion ')' ';' { printf("instruccion_bucle -> do_while"); }
  | PARA '(' asignacion ';' expresion ';' asignacion ')' accion { printf("instruccion_bucle -> for"); }
  | PARA CADA IDENTIFICADOR '(' expresion ')' accion { printf("instruccion_bucle -> for_each"); }
;

instruccion_salto
  : SALTAR IDENTIFICADOR ';' | CONTINUAR ';' | ESCAPE ';' { printf("instruccion_salto -> salto"); }
;

instruccion_destino_salto
  : ETIQUETA IDENTIFICADOR ';' { printf("instruccion_destino_salto -> destino_salto"); }
;

instruccion_devolver
  : DEVOLVER expresion_opcional ';' { printf("instruccion_devolver -> return"); }
;

expresion_opcional
  :
  | expresion           { printf("expresion_opcional -> opcional"); }
;

instruccion_vacia
  : ';'                 { printf("instruccion_vacia -> vacio"); }
;

instruccion_lanzamiento_excepcion
  : LANZA EXCEPCION IDENTIFICADOR ';'       { printf("accion -> instruccion"); }
;

instruccion_captura_excepcion
  : EJECUTA bloque_instrucciones clausulas    { printf("instruccion_captura_excepcion -> captura_excepcion"); }
;

clausulas
  : clausulas_excepcion clausula_defecto        { printf("clausulas -> clausulas_excepcion"); }
  | clausula_defecto                            { printf("clausulas -> clausula_defecto"); }
;

clausulas_excepcion
  : clausulas_excepcion_especifica              { printf("clausulas_excepcion -> clausulas_excepcion_especifica"); }
  | clausulas_excepcion_general                 { printf("clausulas_excepcion -> clausulas_excepcion_general"); }
;

clausulas_excepcion_especifica
  : EXCEPCION IDENTIFICADOR bloque_instrucciones    { printf("clausulas_excepcion_especifica -> excepcion_especifica"); }
;

clausulas_excepcion_general
  : OTRA EXCEPCION bloque_instrucciones           { printf("clausulas_excepcion_general -> excepcion_general"); }
;

clausula_defecto
  : DEFECTO bloque_instrucciones                    { printf("clausula_defecto -> defecto"); }
;

*/

/***************/
/* expresiones */
/***************/

lista_expresiones: expresion
  | lista_expresiones '\n' expresion
;


expresion
  : expresion_constante
  | expresion_indexada
  | expresion_basica
  | expresion_funcional 
  | expresion_logica expresion_sino {printf("	expresion -> expresion_logica\n");}
;

expresion_constante  
	: CTC_ENTERA                 	{printf("	expresion_constante -> CTC_ENTERA\n");}                              
	| CTC_REAL 						        {printf("	expresion_constante -> CTC_REAL\n");}
	| CTC_CADENA 					        {printf("	expresion_constante -> CTC_CADENA\n");}
	| CTC_CARACTER 					      {printf("	expresion_constante -> CTC_CARACTER\n");}
;

//PREGUNTAR COMO METER SIMBOLOS '^.' etc. Si tenemos que declararlos en el Flex o de otra manera
expresion_indexada
  : expresion_basica  {printf("	expresion_indexada -> expresion_basica\n");}
  | expresion_indexada '.' expresion_basica {printf("	expresion_indexada ->  .expresion_basica\n");}
  | expresion_indexada INDIRECCION expresion_basica {printf("	expresion_indexada -> INDIRECCION expresion_basica\n");}
  | expresion_indexada expresion_indireccion indice {printf("	expresion_indexada -> INDIRECCION indice\n");}
;	

expresion_indireccion
  :
  | INDIRECCION {printf("	expresion_indirección -> INDIRECCION\n");}
;

expresion_basica
  : IDENTIFICADOR {printf("	expresion_basica -> IDENTIFICADOR\n");}
  | '(' expresion_constante ')' {printf("	expresion_basica -> expresion_constante\n");}
  | '^' expresion_basica  {printf("	expresion_basica -> expresion_basica\n");}
  | '\\' expresion_basica {printf("	expresion_basica -> expresion_basica\n");}
;

indice
  : '[' expresion_constante ']'   {printf("	indice -> expresion_constante\n");}
  | '{' expresion_constante '}' {printf("	indice -> expresion_constante\n");}
;

expresion_funcional
  : IDENTIFICADOR '(' expresion_asterisco ')' {printf("	expresion_funcional -> expresion_funcional\n");} 
  //aqui es expresion*
;

expresion_asterisco
  : 
  | expresion_asterisco expresion {printf("	expresion_asterisco\n");}
;

/*expresion
  : expresion_logica expresion_sino {printf("	expresion -> expresion_logica\n");}
;*/

expresion_sino
  : 
  | SI expresion SINO expresion {printf("	expresion_sino\n");}
;


expresion_logica 												
  : expresion_or													{printf ("	expresion_logica -> expresion\n");} //aqui necesitamos saber como vai eso do si e o sino PROI
;

expresion_nonassoc
  : '-'             {printf("tamano -> '-'\n");}
  | '~'             {printf("complemento -> '~'\n");}
  | '!'             {printf("negacion_logica -> '!'\n");}
  | TAMANO          {printf("tamano -> tamano\n");}

expresion_potencia
  : expresion_potencia POTENCIA {printf("expresion_potencia -> expresion_potencia '**'\n");}
  | expresion_nonassoc {printf("expresion_potencia -> expresion_nonassoc\n");}
;

operador_multiplicidades
  : '*' {printf("operadores_multiplicidades -> '*'\n");}
  | '/' {printf("operadores_multiplicidades -> '/'\n");}
  | '%' {printf("operadores_multiplicidades -> '%%'\n");}
;

expresion_multiplicidades
  : expresion_multiplicidades operador_multiplicidades expresion_potencia {printf("expresion_multiplicidades -> expresion_multiplicidades operador_multiplicidades expresion_potencia\n");}
  | expresion_potencia {printf("expresion_multiplicidades -> expresion_potencia\n");}
;

operador_suma_resta
  : '+' {printf("operador_suma_resta -> '+'\n");}
  | '-' {printf("operador_suma_resta -> '-'\n");}
;

expresion_suma_resta
  : expresion_suma_resta operador_suma_resta expresion_multiplicidades {printf("expresion_suma_resta -> expresion_suma_resta operador_suma_resta expresion_multiplicidades\n");}
  | expresion_multiplicidades {printf("expresion_suma_resta -> expresion_multiplicidades\n");}
;

operador_desplazamiento
  : FLECHA_DCHA {printf("operador_desplazamiento -> '->'\n");}
  | FLECHA_IZDA {printf("operador_desplazamiento -> '<-'\n");}
;

expresion_desplazamiento
  : expresion_desplazamiento operador_desplazamiento expresion_suma_resta {printf("expresion_desplazamiento -> expresion_desplazamiento operador_desplazamiento expresion_suma_resta\n");}
  | expresion_suma_resta {printf("expresion_desplazamiento -> expresion_suma_resta\n");}
;

expresion_and
  : expresion_and '&' expresion_desplazamiento {printf("expresion_and -> expresion_and '&' expresion_desplazamiento\n");}
  | expresion_desplazamiento {printf("expresion_and -> expresion_desplazamiento\n");}
;

expresion_xor
  : expresion_xor '^' expresion_and {printf("expresion_xor -> expresion_xor '^' expresion_and\n");}
  | expresion_and {printf("expresion_xor -> expresion_and\n");}
;

expresion_or
  : expresion_or '|' expresion_xor {printf("expresion_or -> expresion_or '|' expresion_xor\n");}
  | expresion_xor {printf("expresion_or -> expresion_xor\n");}
;

operador_comparacion
  : '<' {printf("operador_comparacion -> '<'\n");}
  | '>' {printf("operador_comparacion -> '>'\n");}
  | LE {printf("operador_comparacion -> '<='\n");}
  | GE {printf("operador_comparacion -> '>='\n");}
;

expresion_comparacion
  : expresion_comparacion operador_comparacion expresion_or {printf("expresion_comparacion -> expresion_comparacion operador_comparacion expresion_binlog\n");}
  | expresion_or {printf("expresion_comparacion -> expresion_or\n");}
;

operador_igualdad
  : EQ {printf("operador_igualdad -> '=='\n");}
  | NEQ {printf("operador_igualdad -> '<>'\n");}
;

expresion_igualdad
  : expresion_igualdad operador_igualdad expresion_comparacion {printf("expresion_igualdad -> expresion_igualdad operador_ig expresion_comparacion\n");}
  | expresion_comparacion {printf("expresion_igualdad -> expresion_comparacion\n");}
;

expresion_and
  : expresion_and AND expresion_igualdad {printf("expresion_and -> expresion_and AND expresion_igualdad\n");}
  | expresion_igualdad {printf("expresion_and -> expresion_comparacion\n");}
;

expresion_or
  : expresion_or OR expresion_and {printf("expresion_or -> expresion_or OR expresion_and\n");}
  | expresion_and {printf("expresion_or -> expresion_and\n");}
;

expresion_logica
  : expresion_or {printf("expresion_logica -> expresion_or\n");}
;

expresion
  : expresion_logica {printf("expresion -> expresion_logica\n");}
  | expresion_logica '?' expresion ':' expresion {printf("expresion -> expresion_logica '?' expresion ':' expresion\n");}
;



%%

int yyerror(char *s) {
  fflush(stdout);
  printf("*****************, %s, en línea %d\n",s, num_linea);
  }

int yywrap() {
  return(1);
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./mini NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    //printf("  El valor de yyparse ANTES es: %d\n", yyparse());
    yyparse();
    //printf("  El valor de yyparse DESPUES es: %d\n", yyparse());

    }
  }
