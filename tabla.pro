
/*
* ======= Problema de la tabla =======
*
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operadores!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
:- op(500,yfx,:--:). % Auxiliar: usado porque no se trabajar con strings (hay errores al tratar de leer :\/: y :/\: por los caracteres de escape) 
:- op(400,yfx,:++:). % Auxiliar: usado porque no se trabajar con strings (hay errores al tratar de leer :\/: y :/\: por los caracteres de escape)⌈
:- op(500,yfx,:\/:). % Logical OR
:- op(400,yfx,:/\:). % Logical AND
:- op(200,fy,:~:).   % Logical NOT


/*
token(X) triunfa si X no es un operador logico, i.e: es un token identificador.
*/
token(X) :- \+ X = (_:/\:_), \+ X = (_:\/:_), \+ X = (:~:_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Funciones relacionadas a la tabla de simbolos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
bind(X,V,XS,ST) triunda si ST es una tabla de simbolos que contiene a la relacion: (X:V) y todas
las relaciones de XS.
*/
bind(X,V,XS,[symbol(X,V) | XS]).

/*
lookup(X,XS,V) triunfa si el par (X:V) pertenece a la tabla de simbolos.
*/
lookup(X,[symbol(X,V) | _], V).
lookup(X,[_ | RS], V) :- lookup(X,RS,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Funciones relacionadas al AST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
eval(X,Val,ST_,ST) triunfa si Val es el resultado de evaluar la expresion X partiendo el ambiente/tabla de 
simbolos ST_, resultando en la nueva tabla de simbolos ST.
*/
eval(X,Val,ST,ST)  :- token(X), lookup(X,ST,Val),!.
eval(X,1,ST_,ST)   :- token(X), bind(X,1,ST_,ST).
eval(X,0,ST_,ST)   :- token(X), bind(X,0,ST_,ST).
eval(:~:AST,Val,ST_,ST)       :- eval(AST,Val1,ST_,ST), Val is (Val1 + 1) mod 2.
eval(LAST:\/:RAST,Val,ST_,ST) :- eval(LAST,Val1,ST_,ST1), eval(RAST,Val2,ST1,ST), Val is max(Val1,Val2).
eval(LAST:/\:RAST,Val,ST_,ST) :- eval(LAST,Val1,ST_,ST1), eval(RAST,Val2,ST1,ST), Val is min(Val1,Val2).


/*
mapping(X,Y) triunfa si todas las ocurrencia de los functores :\/: y :/\: en X son reemplaazados
por los functores :--: y :++: respectivamente.
*/
mapping(P,P)               :- token(P).
mapping(L:\/:R,L_ :--: R_) :- mapping(L,L_), mapping(R,R_).
mapping(L:/\:R,L_ :++: R_) :- mapping(L,L_), mapping(R,R_).
mapping(:~:R,:~:R_)        :- mapping(R,R_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Funciones relacionadas al formato para el menu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
getVars(X,XS) triunfa si XS contiene todas las variables de X en el mismo orden que fueron escritas.
*/
getVars(P,[P])     :- token(P).
getVars(L:\/:R,XS) :- getVars(L,L_), getVars(R,R_), append(L_,R_,XS).
getVars(L:/\:R,XS) :- getVars(L,L_), getVars(R,R_), append(L_,R_,XS).
getVars(:~:R,XS)   :- getVars(R,XS).

/*
header(XS) siempre triunfa, tiene como efecto de borde imprimir la cabecera de la tabla
*/
header([])       :- write("|"), nl.
header([H | HS]) :- write("| "), write(H), write(" "), header(HS).

/*
line(XS) triunfa si XS es una lista de numeros los cuales representan las longitudes de cada variable,
tiene como efecto de borde, dibujar los "bordes" de la tabla.
*/
line([]) :- write("+"), nl.
line([WL | WLS]) :- write("+"), WL_ is WL+2, line(WL_), line(WLS), !.
line(N) :- N > 0, !, write("-"), N1 is N-1, line(N1).
line(0).

/*
resToStr(X,Y), provee una forma de mostrar los resultados en forma de string.
*/
resToStr(1,"t").
resToStr(0,"f").

/*
writeLine provee una forma de imprimir una fila de la tabla.
*/
writeLine(_,[],[]) :- write("|"), nl.
writeLine(Env, [PadL/PadR | Ps],[Var | Vs]) 
    :- write("| ")
    , tab(PadL)
    , lookup(Var,Env,X)
    , resToStr(X,X_)
    , write(X_)
    , tab(PadR)
    , write(" ")
    , writeLine(Env,Ps,Vs)
    . 

/*
getPads provee una forma de encontrar cuanto padding a izquierda y derecha se necesita
para alinear al centro los valor de "t" y "f" de la tabla.
*/
getPads([],[],[]).
getPads([Token | Tokens], [LPad/RPad | Pads], [Length | Lengths]) 
    :- getPads(Tokens,Pads,Lengths)
    , atom_length(Token, Length)
    , LPad is Length // 2
    , RPad is max(0,Length - LPad - 1) 
    . 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Funciones relacionadas al menu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
process(AST) triunfa si AST es una expresion booleana valida, y tiene como
efecto de borde, imprimir una tabla de verdad, la cual esta formateada segun
el modificador: simple | pro
*/
process(AST,Mode) 
    :- mapping(AST,AST_)
    ,  term_string(AST_,S)
    ,  term_string(AST,S_)
    ,  getVars(AST,Vars)
    ,  moded_append(Mode,Vars,S_,S,Headers_,Headers_1)
    ,  getPads(Headers_1,Pads,Lengths)
    ,  line(Lengths)
    ,  header(Headers_)
    ,  line(Lengths)
    ,  !
    ,  rows(AST,Pads,Vars)
    ,  line(Lengths)
    . 

/*
mode_append ayuda a elegir que estilo de tabla obtenemos.
*/
moded_append(pro,Vars,S_,S,Headers_,Headers_1) 
    :- append(Vars,[S_],Headers_)
    ,  append(Vars,[S],Headers_1)
    .
moded_append(simple,Vars,_,_,Headers_,Headers_) 
    :- append(Vars,["F"],Headers_)
    . 

/*
rows evalua todas los posibles estados de la tabla, e imprime las filas.
*/
rows(AST,Pads, Vars) 
    :- eval(AST,Sol,[],Env)
    ,  Env_ = [symbol(:^:,Sol) | Env] % necesitamos un simbolo exclusivo para la respuesta.... might as well be un operador
    ,  append(Vars,[:^:],Vars_) 
    ,  writeLine(Env_,Pads,Vars_)
    ,  fail
    . 
rows(_,_,_).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MENU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

genTable(Mode)
    :- repeat  
    , write("formula: ")
    , read(X)
    , decider(X,Mode)
    , !.

decider(bye,_).
decider(AST,Mode) :- process(AST,Mode), fail.


truthtable :- genTable(simple).
truthtablepro :- genTable(pro).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parser Adicional.... Hay que modificarlo porque usa la precedencia de prolog en vez de la del pdf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
star(X,XS) parsea 0 o mas veces X, acumulando lo que este retorne en XS.
*/
star(_,[]) --> [].
star(X,XS) --> call(X,Y), star(X,XS_), {XS = [Y | XS_]}.

/*
unaryOperator parsea un operador unario y lo retorna.
*/
unaryOperator(OP) --> [:~:], {OP = (:~:)}.

/*
p2Operator parsea un operador de precedencia 2 y lo retorna.
*/
p2Operator(OP)    --> [:/\:], {OP=(:/\:)} | [:\/:], {OP=(:\/:)}.

/*
Gramatica BNFE asociada:

e    -> p2
p2   -> p3 ((:/\: | :\/:) p3)*
p3   -> (:~:)* term
term -> '('e')' | (pseudo)terminal.
*/

/*
Estructura del AST:

AST = unary(<Unary Operator>, AST) 
    | binary(<binary Operator>, LAST, RAST)
    | leaf(<pseudo terminal>)
*/

e(AST)         --> p2(AST).
p2(AST)        --> p3(AST1), star(p2R,XS), {buildLeftAssocBin(AST1,XS,AST)}.
p2R(OP/AST1)   --> p2Operator(OP), p3(AST1).
p3(AST)        --> star(unaryOperator,UOPs), term(AST1), {buildUnaryTree(UOPs,AST1,AST)}. 
term(AST) 
    --> ['('], e(AST), [')']
    |   [T], {token(T), AST=leaf(T)}
    . 

buildUnary(:~:,AST,:~:AST).
buildBinary(:/\:,L,R,L :/\: R).
buildBinary(:\/:,L,R,L :\/: R).

/*
buildUnaryTree, construye un arbol unario a partir de una lista de operadores unarios y un AST base.
*/
buildUnaryTree([UOP | Rest], AST1, AST) :- buildUnaryTree(Rest,AST1,AST2), buildUnary(UOP,AST2,AST).
buildUnaryTree([],AST,AST).



/*
buildLeftAssocBin construye un ast dado un AST inicial, y una lista de operadores binarios asociativos a 
la izquierda, seguidos de un AST.
*/
buildLeftAssocBin(Acc,[(OP/AST1) | XS],Res) 
    :- buildBinary(OP,Acc,AST1,Acc_)
    ,  buildLeftAssocBin(Acc_,XS,Res).
buildLeftAssocBin(Acc,[],Acc).
 
