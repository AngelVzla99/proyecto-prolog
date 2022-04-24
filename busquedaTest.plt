:- begin_tests(busqueda).
:- consult('busqueda.pro').

test(test1) :- 
    Ans = [push(above, 2), push(below, 1), pop(above, 2), pop(below, 1)],
    vagones( [a,b,c], [b,c,a], Operaciones ),
    assertion( Ans = Operaciones ).

test(test2) :- 
    Ans = [],
    vagones( [a,b,c], [a,b,c], Operaciones ),
    assertion( Ans = Operaciones ).

test(test3) :- 
    Ans = [push(above, 3), push(below, 1), pop(above, 1), push(below, 1), pop(above, 1), push(below, 1), pop(above, 1), pop(below, 3)],
    vagones( [a,b,c,d], [d,c,b,a], Ans ).

test(test4) :- 
    Ans = [4],
    canales( [1,1,1,5],5,L ),
    assertion( L = Ans ).

test(test5) :- 
    Ans = [1,2,3,2,4,2],
    canales( [3,1,3,3] ,5,L ),
    assertion( L = Ans ).

test(test6) :- 
    Ans = [2, 3, 2, 3, 4], 
    canales( [1,4,4,5] ,5,L ),
    assertion( L = Ans ).

:- end_tests(busqueda).

:- run_tests.
