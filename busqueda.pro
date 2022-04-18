
final( matriz, R/C ) :- R=3, C=3.
inicial( matriz, R/C ) :- R = 1, C =1.

movimiento( matriz, _, X/Y ) :- 
    member(X,[0,1,-1]),
    member(Y,[0,1,-1]).

moverse( matriz, X/Y, DX/DY, I/J ) :-
    I is X + DX,
    J is Y + DY.

legal( matriz, X/Y ) :- X>0, Y>0, X<4, Y<4.

endState( Ans, [], Ans2 ) :- Ans = Ans2.
endState( X/Y, [I/J|Resto], Ans ) :-
    XX is X + I,
    YY is Y + J,
    endState( XX/YY, Resto, Ans ).

% ------------------------------------------------------------

ddfs( Problema, Estado, Movimientos ) :- 
    ddfs( Problema, Estado, Movimientos, 0 ).
ddfs( Problema, Raiz, Movimientos, Depth ) :- 
    ddfs( Problema, Depth, Raiz, [Raiz], Movimientos, goal ). 
ddfs( Problema, Raiz, Movimientos, Depth ) :- 
    ddfs( Problema, Depth, Raiz, [Raiz], _, continue ),
    NewDepth is Depth+1,
    ddfs( Problema, Raiz, Movimientos, NewDepth ).

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

resolver( Problema, Movimientos ) :-
    inicial( Problema, Estado ),
    ddfs( Problema, Estado, Movimientos, 0 ).

simular(Problema) :-
    inicial( Problema, Estado ),
    ddfs( Problema, Estado, Movimientos, 0 ),
    mostrar(Problema,Estado,Movimientos).

% ------------------------------------------------------------

/*
* ======= Problema del patio de operaciones =======
*
* Cada estado sera representado como estacion(Lista1,Lista2,Lista3)
* Donde Lista1 es la lista del pie de la Y
* la lista2 representa la esquina superior izquierda de la Y
* la lista3 representa la esquina superior derecha de la Y
*/

:- discontiguous( [ inicial/2, final/2, movimiento/3, moverse/4, legal/2, mostrar/3 ] ).
:- dynamic( [prueba/1, inicial/2, final/2] ).

/*
Triunfa si logra resolver el problema de patio de operaciones 
dado un estado inicial y un estado final unificando la lista de 
operaciones
*/
vagones( Inicial, Final, Operaciones ) :-
    asserta( inicial( vagones, estacion(Inicial,[],[]) ) ),
    asserta( final( vagones, estacion(Final,[],[]) ) ),
    resolver( vagones, Operaciones ), !,
    %simular(vagones),          % descomentar para ver la simulacion
    retract( final(vagones, estacion(Final,[],[])) ),
    retract( inicial(vagones, estacion(Inicial,[],[])) ). 

/*
movimientos validos en el problema de los vagones, usa 
backtracking para tomar elementos de cada una de las 3 listas,
unificando el tercer argumento
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
mostrar( vagones, Estado, [] ) :- mostrar(Estado), !.
mostrar( vagones, Estado, [Move|Moves] ) :-
    mostrar(Estado),
    write("Aplico "), write( Move ), nl,
    moverse( vagones, Estado, Move, NewState ), 
    mostrar( vagones, NewState, Moves ).
mostrar( estacion(L1,L2,L3) ) :-
    write("Estado actual : "), nl, 
    write("Lista2 => \t\t"), write(L2), nl, 
    write("Lista1 => "), write(L1), nl, 
    write("Lista3 => \t\t"), write(L3), nl.

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

% ------------------------------------------------------------
% Problema Control remoto


/*

(3,1) (3,2) (3,3)
(2,1) (2,2) (2,3)
(1,1) (1,2) (1,3)

asserta( prueba(2) ).
listing(prueba).
retract(prueba(_)).

*/