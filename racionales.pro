/*
* ======= Problema de Aritmética Racional =======
*
*/

:- op(700,xfx, isr).  %isr tiene la misma precedencia y asociatividad que is.
:- op(700,xfx, eval). %auxiliar, dado que isr devuelve un entero cuando algo es de la forma A\\1, ayuda a limpiar un poco el codigo.
:- op(300,fy,~).      % menor precedencia que el \\ para que los ejemplos funcionen
:- op(200,xfx,\\).    % no asociativo, mayor precedencia para que los ejemplos funcionen.


/*
cast(A,B) triunfa si A es una fraccion con denominar 1, y B es el numerador de esa fraccion, o si A unifica con B.
*/
cast(A\\1,A).
cast(A,A).

/*
decider(A,B) triunfa si A es 0\\0 y B es not_a_number, si A es unificable con _\\0 y B es infinity, o en caso  de que A no sea unificable
con ninguno, si A es unificable con B.
*/
decider(0\\0,not_a_number) :- !.
decider(_\\0,infinity)     :- !.
decider(A,A)               :- !.

/*
simplify(A,B) triunfa si B es la representacion de fraccion reducida de A, o si A evalua a un entero, y R = A \\ 1.
*/
simplify(A\\B,R)    :- A_ is A, B_ is B, D is max(1,gcd(A_,B_)), Num is A_ // D, Den is B_ // D, decider(Num\\Den,R) , !.
simplify(A,R)       :- A_ is A, R = A_ \\ 1 , !.


% las operaciones go brrr
add(A\\B,C\\D,Num\\Den) :-  Num is A*D + C*B, Den is B*D.
sub(A\\B,C\\D,Num\\Den) :-  Num is A*D - C*B, Den is B*D.
mul(A\\B,C\\D,Num\\Den) :-  Num is A*C      , Den is B*D.
div(A\\B,C\\D,Num\\Den) :-  Num is A*D      , Den is B*C.
flip(A\\B,B\\A).

% envolvemos las evaluaciones en evalOP para no tener tantos casos con los absorbentes.
evalOP(not_a_number,_,_,not_a_number).
evalOP(_,_,not_a_number,not_a_number).
evalOP(infinity,_,_,infinity).
evalOP(_,_,infinity,infinity).
evalOP(L,OP,R,E) :- call(OP,L,R,E_), simplify(E_,E).
evalOP(_,not_a_number,not_a_number).
evalOP(_,infinity,infinity).
evalOP(OP,R,E) :-call(OP,R,E_), simplify(E_,E).

% y envolvemos evalOP sobre eval, para poder trabajar sobre el arbol de manera comoda.
E eval ~A      :- A_ eval A, evalOP(flip,A_,E), !.
E eval (L + R) :- L_ eval L, R_ eval R, evalOP(L_,add,R_,E), !.
E eval (L - R) :- L_ eval L, R_ eval R, evalOP(L_,sub,R_,E), !.
E eval (L * R) :- L_ eval L, R_ eval R, evalOP(L_,mul,R_,E), !.
E eval (L / R) :- L_ eval L, R_ eval R, evalOP(L_,div,R_,E), !.
E eval A       :- simplify(A,E) , !.

% y envolvemos eval sobre isr porque asi eval siempre trabaja con puras fracciones, y el resultado se castea a entero solo al final.
E isr B :- Aux eval B, cast(Aux,E) , !.
