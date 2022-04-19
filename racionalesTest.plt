:- begin_tests(racionales).
:- consult('racionales.pro').

test(test1) 
    :- A isr 20\\14
    , assertion(A == 10\\7) 
    . 
 

test(test2) 
    :- A isr 20\\14 + 4\\7
    , assertion(A == 2) 
    . 

test(test3) 
    :- A isr ~2
    , assertion(A == 1\\2) 
    . 
 
test(test4)
    :- A isr ~2\\3 + 5
    , assertion(A == 13\\2)
    . 

test(test5)
    :- A isr 0 - (2\\5 / 10\\3)
    , assertion(A == (-3)\\25)
    . 

test(test6)
    :- A isr 1\\0
    , assertion(A==infinity)
    . 

test(test7)
    :- A isr 0\\0
    , assertion(A == not_a_number)
    . 

test(test8)
    :- A isr 0\\0 + 1 \\ 2
    , assertion(A == not_a_number)
    . 

test(test9)
    :- A isr 3\\5 / 0\\2
    , assertion(A == infinity)
    . 

test(test10)
    :- A isr 3\\5 / 0\\2 - 1
    , assertion(A == infinity)
    . 

test(test11)
    :- A isr 3\\5 / 0\\2 - 3\\5 / 0\\2
    , assertion(A == infinity)
    . 

:- end_tests(racionales).


:- run_tests.
