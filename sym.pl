polynomial(number([49])) --> x_sym.
polynomial(number(Y)) --> x_sym, power_op, [123], number(Y), [125].
polynomial(number([Y])) --> x_sym, power_op, digit(Y).

expression(number(X)) --> number(X).
expression(fraction(X)) --> fraction(X).
expression(power(X, Y)) --> [40], expression(X), power_op, digit(Y), [41].
expression(power(X, Y)) --> [40], expression(X), power_op, [123], number(Y), [125], [41].
expression(times(X, Y)) --> [40], expression(X), times_op, expression(Y), [41].
expression(plus(X, Y)) -->  [40], expression(X), plus_op, expression(Y), [41].
expression(diff(X, Y)) --> [40], expression(X), diff_op, expression(Y), [41].

coefficient(coeff(X, number([48]))) --> expression(X).
coefficient(coeff(number([48]), Y)) --> polynomial(Y).
coefficient(coeff(X, Y)) --> expression(X), times_op, polynomial(Y).

signed_coefficient(signed(OP, X)) --> adder(OP), coefficient(X).
signed_coefficient(signed(43, X)) --> coefficient(X).

equation([X]) --> signed_coefficient(X).
equation([X|Y]) --> signed_coefficient(X), equation(Y).

complex_equation(equation(L)) --> equation(L).
complex_equation(eqn_times(X, Y)) --> [40], complex_equation(X), times_op, complex_equation(Y), [41].
complex_equation(eqn_plus(X, Y)) -->  [40], complex_equation(X), plus_op, complex_equation(Y), [41].
complex_equation(eqn_diff(X, Y)) --> [40], complex_equation(X), diff_op, complex_equation(Y), [41].

plus_op --> [43].
diff_op --> [45].
times_op --> [42].
times_op --> [92, 116, 105, 109, 101, 115].
power_op --> [94].
x_sym --> [120].

number([D]) --> digit(D).
number([D|N]) --> digit(D), number(N).
number([D|N]) --> digit(D), number(N).
digit(D) --> [D], {D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57}.

fraction([D|N]) --> decimal(D), number(N).
fraction([D|N]) --> digit(D), fraction(N).

decimal(D) --> [D], {D=46}.
adder(D) --> [D], {D=43;D=45}.

%comparator for sorting coefficients
cheaper(<, signed(_,coeff(_,C1)), signed(_,coeff(_,C2))) :- C1>C2.
cheaper(>, signed(_,coeff(_,C1)), signed(_,coeff(_,C2))) :- C1=<C2.

%higher level evaluation
eval(signed(OP, X), signed(OP, X1)) :- eval(X, X1).
eval([X], [X1]) :- eval(X, X1).
eval([X|T], [X1|T1]) :- eval(X, X1), eval(T, T1).
eval(coeff(X, Y), coeff(X1, Y1)) :- eval(X, X1), eval(Y, Y1).

%basic evaluation
eval(plus(E1, E2), V) :- eval(E1, V1), eval(E2, V2), V is V1 + V2.
eval(diff(E1, E2), V) :- eval(E1, V1), eval(E2, V2), V is V1 - V2.
eval(times(E1, E2), V) :- eval(E1, V1), eval(E2, V2), V is V1 * V2.
eval(number(L), V) :- reverse(L, LL), subeval(LL, V).
eval(fraction(L), R) :- reverse(L, LL), get_int(LL, I), get_frac(L, F), R is I + 0.1*F.

subeval([D], V) :- !, (D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57), V is (D-48).
subeval([D|L], V) :- subeval(L, VV), V is ((VV*10) + (D-48)).

get_int_helper([D], V) :- !, (D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57), V is (D-48).
get_int_helper([D|L], V) :- get_int_helper(L, VV), V is ((VV*10) + (D-48)).
get_int([D|L], V) :- (D=46) -> get_int_helper(L, VV), V is VV; get_int(L, V).

get_frac_helper([D], V) :- !, (D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57), V is (D-48).
get_frac_helper([D|L], V) :- get_frac_helper(L, VV), V is ((VV*0.1) + (D-48)).
get_frac([D|L], V) :- (D=46) -> get_frac_helper(L, VV), V is VV; get_frac(L, V).

%For Q1a
parse_and_print :- process, !, parse_and_print.
parse_and_print.
process :- readline(L), exclude([X]>>(X =:= 32), L, RL), continue(RL).
readline(L) :- current_input(S), read_line_to_codes(S, L).
continue(end_of_file) :- nl, writeln("End"), !, fail.
continue(L) :- equation(E, L, []), eval(E, R), predsort(cheaper, R, RR), writeln(RR).
continue(_) :- writeln("Invalid format").

start(File1, File2):-
    open(File1, read, Str1),
    open(File2, write, Str2),
    process_file(Str1, Str2),
    close(Str1),
    close(Str2).

process_file(Str1, _) :-
    at_end_of_stream(Str1).

process_file(Str1,Str2) :-
    \+ at_end_of_stream(Str1),
    read_line_to_codes(Str1, X),
    %eval(X, R),
    writeln(Str2, X),
    process_file(Str1, Str2).


%start(File1, File2) :- open(File1, read, Stream1)
