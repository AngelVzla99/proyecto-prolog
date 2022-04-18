/* 
--------------------------- 
-- Implementacion de IDS  | 
--------------------------- 
*/

/*
ddfs( Problema, Estado Movimiento ) triunfa si utilizando IDS se 
logra encontrar un estado final, este algoritmo hace 2 pasadas 
por cada profundidad, una buscando el 'goal' es decir, un estado
final a profundidad 'depth' y una segunda pasada buscando 'continue'
para saber si existen hojas sin ver a profundidad 'depth'
*/
ddfs( Problema, Estado, Movimientos ) :- 
    ddfs( Problema, Estado, Movimientos, 0 ). % Este lo puse porque lo dice el enunciado, pero no se usa
ddfs( Problema, Raiz, Movimientos, Depth ) :- 
    ddfs( Problema, Depth, Raiz, [Raiz], Movimientos, goal ), !. 
ddfs( Problema, Raiz, Movimientos, Depth ) :- 
    ddfs( Problema, Depth, Raiz, [Raiz], _, continue ),
    NewDepth is Depth+1,
    ddfs( Problema, Raiz, Movimientos, NewDepth ), !.

/*
Esta funcion es muy parecida a la de dfs que vimos en clase, la 
diferencia es que toma en cuenta la profundidad para encontrar
un estado final
*/
ddfs( Problema, 0, Estado, _, [], goal) :- final(Problema,Estado).
ddfs( _, 0, _, _, [], continue).
ddfs( Problema, Profundidad, Estado, Historia, [Movimiento|Movimientos], DecisionHoja ) :-
    Profundidad > 0,
    NuevaProfundidad is Profundidad - 1, 
    movimiento( Problema, Estado, Movimiento ), 
    moverse( Problema, Estado, Movimiento, Proximo ),
    legal(Problema, Proximo),
    \+member(Proximo,Historia),
    ddfs(Problema,NuevaProfundidad,Proximo,[Proximo|Historia],Movimientos,DecisionHoja).

/*
resolver utiliza ddfs para resolver un problema determinado usando
los predicados necesarios para ddfs. Unificando los movimientos
utilizados para llegar a ese estado
*/
resolver( Problema, Movimientos ) :-
    inicial( Problema, Estado ), !,
    ddfs( Problema, Estado, Movimientos, 0 ).

/*
Este predicado es similar a resolver, la diferencia es que imprime
por standar output 
*/
simular(Problema) :-
    inicial( Problema, Estado ),
    ddfs( Problema, Estado, Movimientos, 0 ),
    mostrar(Problema,Estado,Movimientos).

/*
Este predicado es generico para cualquier problema resuelto con 
ddfs y se encarga de imprimir por pantalla los pasos necesarios 
para llegar a la solucion
*/
mostrar( _, Estado, [] ) :- mostrar(Estado), !.
mostrar( Problema, Estado, [Move|Moves] ) :-
    mostrar(Estado),
    write("Aplico "), write( Move ), nl,
    moverse( Problema, Estado, Move, NewState ), 
    mostrar( Problema, NewState, Moves ).


:- discontiguous( [ inicial/2, final/2, movimiento/3, moverse/4, legal/2, mostrar/3, mostrar/1 ] ).
:- dynamic( [prueba/1, inicial/2, final/2] ).

/* ================================================================
-----------------------------------------
-- Problema del patio de operaciones    |
-----------------------------------------

* Cada estado sera representado como estacion(Lista1,Lista2,Lista3)
* Donde Lista1 es la lista del pie de la Y
* la lista2 representa la esquina superior izquierda de la Y
* la lista3 representa la esquina superior derecha de la Y
*/

/*
Triunfa si logra resolver el problema de patio de operaciones 
dado un estado inicial y un estado final unificando la lista de 
operaciones
*/
vagones( Inicial, Final, Operaciones ) :-
    permutation(Inicial, Final),
    asserta( inicial( vagones, estacion(Inicial,[],[]) ) ),
    asserta( final( vagones, estacion(Final,[],[]) ) ),
    resolver( vagones, Operaciones ),
    %simular(vagones),          % descomentar para ver la simulacion
    retract( final(vagones, estacion(Final,[],[])) ),
    retract( inicial(vagones, estacion(Inicial,[],[])) ), !. 

/*
movimientos validos en el problema de los vagones, usa 
backtracking para tomar elementos de cada una de las 3 listas,
unificando el tercer argumento, prueba las 4 opciones 
*/
movimiento( vagones, estacion(L1,_,_), push(above,X) ) :- 
    length(L1, Len), between(1,Len,X). 
movimiento( vagones, estacion(L1,_,_), push(below,X) ) :- 
    length(L1, Len), between(1,Len,X). 
movimiento( vagones, estacion(_,L2,_), pop(above,X) ) :- 
    length(L2, Len), between(1,Len,X). 
movimiento( vagones, estacion(_,_,L3), pop(below,X) ) :- 
    length(L3, Len), between(1,Len,X). 

/*
Triunfa si logra hacer el movimiento desde el estado actual al nuevo
unifica creando un nuevo estado en el ultimo parametro
*/
moverse( vagones, estacion(L1,L2,L3), push(above,X), Proximo ) :- 
    popBack( X, L1, Vagon, NewL1 ),
    append( Vagon, L2, NewL2 ),
    Proximo = estacion( NewL1, NewL2, L3 ).
moverse( vagones, estacion(L1,L2,L3), push(below,X), Proximo ) :- 
    popBack( X, L1, Vagon, NewL1 ),
    append( Vagon, L3, NewL3 ),
    Proximo = estacion( NewL1, L2, NewL3 ).
moverse( vagones, estacion(L1,L2,L3), pop(above,X), Proximo ) :- 
    popFront( X, L2, Vagon, NewL2 ),
    append( L1, Vagon, NewL1 ),
    Proximo = estacion( NewL1, NewL2, L3 ). 
moverse( vagones, estacion(L1,L2,L3), pop(below,X), Proximo ) :- 
    popFront( X, L3, Vagon, NewL3 ),
    append( L1, Vagon, NewL1 ),
    Proximo = estacion( NewL1, L2, NewL3 ). 

% Por como se hace el movimiento todos los estados son validos
legal(vagones, _). 

% Predicado para mostrar la solucion del problema de los vagones
mostrar( estacion(L1,L2,L3) ) :-
    write("Estado actual : "), nl, 
    write("Lista2 => \t\t"), write(L2), nl, 
    write("Lista1 => "), write(L1), nl, 
    write("Lista3 => \t\t"), write(L3), nl.

% ====:> Funciones Auxiliares <:=====

/*
popFront(N,L1,Take,Rest) triunfa si se logran tomar N elementos
de izquierda a derecha, unificando los elementos tomados en Take
y el resto de L1 en Rest
*/
popFront( 0, L, [], Rest ) :- Rest = L, !.
popFront( N, [X|Resto], [X|Resto2], Quedan ) :-
    N1 is N-1,
    popFront( N1, Resto, Resto2, Quedan ).

/*
popBack(N,L1,Take,Rest) triunfa si se logran tomar N elementos
de derecha a izquierda, unificando los elementos tomados en Take
y el resto de L1 en Rest
*/
popBack( N, L, Taken, Rest ) :-
    reverse(L,L2), 
    popFront( N, L2, Taken2, Rest2 ),
    reverse( Taken2, Taken ),
    reverse( Rest2, Rest ).

/* ================================================================
-----------------------------------------
-- Problema de los controles remotos    |
-----------------------------------------

* Representare los estados como: televisor(N,T1,T2,T3,T4,Previo)
* Donde N es el numero de canales de television posibles
* Ti es el canal que sintoniza el i-esimo televisor y Previo
* es el ultimo televisor donde se cambio el canal
*/

% Triunfa si en estado actual todos los televisores sintonizan lo mismo
final( canales, televisores(_,T1,T2,T3,T4,_) ) :- T1=T2, T2=T3, T3=T4.

/*
canales( Lista4, N, Operaciones ) triunfa unificando su tercer 
argumento con una lista. La lista describe el orden en que hay 
que aplicar «Next» a cada televisor, para que al final terminen
sintonizados en el mismo canal. Más aún debe ser la secuencia
 mínima de pasos.
*/
canales( [T1,T2,T3,T4], N, Operaciones ) :-
    asserta( inicial( canales, televisores(N,T1,T2,T3,T4,-1) ) ),
    resolver( canales, Operaciones ),
    %simular(canales),          % descomentar para ver la simulacion
    retract( inicial( canales, televisores(N,T1,T2,T3,T4,-1) ) ), !. 

/*
movimiento( canales, televisores(...), X ) triunfa si X
es un movimiento valido (entre 1 y 4 y diferente del ultimo televisor
cambiado)
*/
movimiento( canales, televisores(_,_,_,_,_,Last), X ) :- 
    between(1,4,X), \+(X=Last).

/*
el predicado moverse(canales, televisores(...), i, Proximo)  triunfa
unificando en proximo si este es el estado proximo tras usar el boton
de Next en el i-esimo televisor
*/
moverse( canales, televisores(N,T1,T2,T3,T4,_), 1, Proximo ) :- 
    NewT1 is mod(T1+1,N),
    Proximo = televisores(N,NewT1,T2,T3,T4,1). 
moverse( canales, televisores(N,T1,T2,T3,T4,_), 2, Proximo ) :- 
    NewT2 is mod(T2+1,N),
    Proximo = televisores(N,T1,NewT2,T3,T4,2). 
moverse( canales, televisores(N,T1,T2,T3,T4,_), 3, Proximo ) :- 
    NewT3 is mod(T3+1,N),
    Proximo = televisores(N,T1,T2,NewT3,T4,3). 
moverse( canales, televisores(N,T1,T2,T3,T4,_), 4, Proximo ) :- 
    NewT4 is mod(T4+1,N),
    Proximo = televisores(N,T1,T2,T3,NewT4,4). 

% Por como se hace el movimiento todos los estados son validos
legal(canales, _). 
    
% Predicado para mostrar la solucion del problema de los televisores
mostrar( televisores(_,T1,T2,T3,T4,_) ) :-
    write("Estado actual : "), nl, 
    write([T1,T2,T3,T4]), nl.
