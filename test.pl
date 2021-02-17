expression(number(X)) -->  number(X).
expression(plus(X, Y)) --> [40], expression(X), plus_op, expression(Y), [41].
expression(times(X, Y)) --> [40], expression(X), times_op, expression(Y), [41].

plus_op --> [43].
times_op --> [42].

number([D]) --> digit(D).
number([D|N]) --> digit(D), number(N).
digit(D) --> [D], {D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57}.

eval(plus(E1, E2), V) :- eval(E1, V1), eval(E2, V2), V is V1 + V2.
eval(times(E1, E2), V) :- eval(E1, V1), eval(E2, V2), V is V1 * V2.

eval(number(L), V) :- reverse(L, LL), subeval(LL, V).
subeval([D], V) :- !, (D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57), V is (D-48).
subeval([D|L], V) :- subeval(L, VV), V is ((VV*10) + (D-48)).

go :- process, !, go.
go.
process :- readline(L), continue(L).
readline(L) :- current_input(S), read_line_to_codes(S, L).
continue(end_of_file) :- nl, writeln("Good bye"), !, fail.
continue(L) :- expression(E, L, []), eval(E, V), writeln(V).
continue(_) :- writeln("I don't understand").
