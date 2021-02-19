polynomial(number([49])) --> x_sym.
polynomial(number(Y)) --> x_sym, power_op, [123], number(Y), [125].
polynomial(number([Y])) --> x_sym, power_op, digit(Y).

expression(number(X)) --> number(X).
expression(float(X)) --> float(X).
expression(frac(number(X), number(Y))) --> frac_symbol, [123], number(X), [125], [123], number(Y), [125].
expression(power(X, Y)) --> [40], expression(X), power_op, digit(Y), [41].
expression(power(X, Y)) --> [40], expression(X), power_op, [123], number(Y), [125], [41].
expression(times(X, Y)) --> [40], expression(X), times_op, expression(Y), [41].
expression(plus(X, Y)) -->  [40], expression(X), plus_op, expression(Y), [41].
expression(minus(X, Y)) --> [40], expression(X), minus_op, expression(Y), [41].

coefficient(coeff(X, number([48]))) --> expression(X).
coefficient(coeff(number([48]), Y)) --> polynomial(Y).
coefficient(coeff(X, Y)) --> expression(X), times_op, polynomial(Y).

signed_coefficient(signed(OP, X)) --> adder(OP), coefficient(X).

equation([X]) --> signed_coefficient(X).
equation([X|Y]) --> signed_coefficient(X), equation(Y).

complex_equation(cmplx_eqn(L)) --> equation(L).
complex_equation(eqn_times(X, Y)) --> [91], complex_equation(X), [93], times_op, [91], complex_equation(Y), [93].
complex_equation(eqn_plus(X, Y)) --> [91], complex_equation(X), [93], plus_op, [91], complex_equation(Y), [93].
complex_equation(eqn_minus(X, Y)) --> [91], complex_equation(X), [93], minus_op, [91], complex_equation(Y), [93].

complex_equation(diff(X)) --> diff_symbol, [123], complex_equation(X), [125], [123], x_sym, [125].

plus_op --> [43].
minus_op --> [45].
times_op --> [42].
times_op --> [92, 116, 105, 109, 101, 115].
power_op --> [94].
x_sym --> [120].
diff_symbol --> [92, 100, 105, 102, 102].
frac_symbol --> [92, 102, 114, 97, 99].


number([D]) --> digit(D).
number([D|N]) --> digit(D), number(N).
number([D|N]) --> digit(D), number(N).
digit(D) --> [D], {D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57}.

float([D|N]) --> decimal(D), number(N).
float([D|N]) --> digit(D), float(N).

decimal(D) --> [D], {D=46}.
adder(D) --> [D], {D=43;D=45}.

% comparator for sorting coefficients
cheaper(<, coeff(_,C1), coeff(_,C2)) :- C1>C2.
cheaper(>, coeff(_,C1), coeff(_,C2)) :- C1=<C2.

% simplification
simplify(L, simple(RR)):- simplify_acc(L, [], R), reverse(R, RR).
simplify_acc([], Acc, Acc).
simplify_acc([coeff(A,C)|T1], [coeff(B,C)|T2], Res) :- 
        eval(plus(A,B), R),
        ((number(R), R=:=0) -> 
            simplify_acc(T1, T2, Res); 
            simplify_acc(T1, [coeff(R,C)|T2], Res)).
simplify_acc([X|T1], Acc, Res) :- simplify_acc(T1, [X|Acc], Res).

% function helper for eqn product
eqn_times_single(_L1, [], Acc, Acc).
eqn_times_single(coeff(A,B), [coeff(C,D)|T], Acc, Res) :- eqn_times_single(coeff(A,B), T, [coeff(E,F)|Acc], Res), eval(times(A,C),E), F is B+D.

eqn_times_list([], _L2, Acc, Acc).
eqn_times_list([H|T], L2, Acc, Res) :-
    eqn_times_single(H, L2, [], R1),
    append(Acc, R1, R3),
    eqn_times_list(T, L2, R3, Res).

eqn_times_fn(simple(L1), simple(L2), R) :- eqn_times_list(L1, L2, [], R).

% function for differentiation
differentiate(simple(L), simple(RR)):- differentiate_acc(L, [], R), reverse(R, RR).

differentiate_acc([], Acc, Acc).
differentiate_acc([coeff(A,B)|T], Acc, Res) :-
    (B > 0) ->
        C is A*B,
        D is B-1,
        differentiate_acc(T, [coeff(C,D)|Acc], Res);
        differentiate_acc(T, Acc, Res).
    
frac_times_int(frac(A, B), C, R) :- D is A*C, eval(frac(D, B), R).

% eval a simplified eqn
eval(simple(L), simple(L)).

% fraction evaluation
eval(X, R) :- number(X) -> R is X; false.

eval(frac(X, Y), R) :- eval(X, X1), eval(Y, Y1), gcd(X1, Y1, Y1), R is X1/Y1.
eval(frac(X, Y), frac(XR, YR)) :- eval(X, X1), eval(Y, Y1), gcd(X1, Y1, Z), XR is X1/Z, YR is Y1/Z.

eval(plus(frac(X1, Y1), frac(X2, Y2)), R) :- eval(X1, A), eval(Y1, B), eval(X2, C), eval(Y2, D), E is A*D+B*C, F is B*D, eval(frac(E, F), R).

eval(times(frac(X1, Y1), frac(X2, Y2)), R) :- 
    eval(X1, A), eval(Y1, B), 
    eval(X2, C), eval(Y2, D), 
    E is A*C, F is B*D, eval(frac(E, F), R).

eval(times(frac(X1, Y1), Z), R) :- 
    eval(frac(X1, Y1), frac(A, B)), 
    eval(Z, C), 
    frac_times_int(frac(A, B), C, R).

eval(times(Z, frac(X1, Y1)), R) :- 
    eval(times(frac(X1, Y1), Z), R).

% complex eqn evaluation
eval(eqn_times(X, Y), R) :- 
    eval(X, XR), 
    eval(Y, YR),
    eqn_times_fn(XR, YR, R1),
    predsort(cheaper, R1, R2),
    simplify(R2, R).

eval(diff(X), R) :-
    eval(X, X1),
    differentiate(X1, R).

% higher level evaluation
eval(cmplx_eqn(L), R) :- eval(L, L1), predsort(cheaper, L1, L2), simplify(L2, R).
eval(signed(OP, X), R) :- eval(X, X1), eval(OP, X1, R).
eval([X], [X1]) :- eval(X, X1).
eval([X|T], [X1|T1]) :- eval(X, X1), eval(T, T1).
eval(coeff(X, Y), coeff(X1, Y1)) :- eval(X, X1), eval(Y, Y1).

% basic evaluation
eval(plus(E1, E2), V) :- eval(E1, V1), eval(E2, V2), number(V1), number(V2), V is V1 + V2.
eval(minus(E1, E2), V) :- eval(E1, V1), eval(E2, V2), V is V1 - V2.
eval(times(E1, E2), V) :- eval(E1, V1), eval(E2, V2), V is V1 * V2.
eval(number(L), V) :- reverse(L, LL), subeval(LL, V).
eval(float(L), R) :- reverse(L, LL), get_int(LL, I), get_float(L, F), R is I + 0.1*F.

eval(43, X, X).
eval(45, coeff(frac(A, B), Y), coeff(frac(A1, B), Y)) :- A1 is -1*A.
eval(45, coeff(X, Y), coeff(X1, Y)) :- X1 is -1*X.

subeval([D], V) :- !, (D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57), V is (D-48).
subeval([D|L], V) :- subeval(L, VV), V is ((VV*10) + (D-48)).

get_int_helper([D], V) :- !, (D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57), V is (D-48).
get_int_helper([D|L], V) :- get_int_helper(L, VV), V is ((VV*10) + (D-48)).
get_int([D|L], V) :- (D=46) -> get_int_helper(L, VV), V is VV; get_int(L, V).

get_float_helper([D], V) :- !, (D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57), V is (D-48).
get_float_helper([D|L], V) :- get_float_helper(L, VV), V is ((VV*0.1) + (D-48)).
get_float([D|L], V) :- (D=46) -> get_float_helper(L, VV), V is VV; get_float(L, V).

%function helpers for fractions
gcd(X, Y, Z) :-
    X < 0, !,
    gcd(-X, Y, Z).
gcd(X, Y, Z) :-
    Y < 0, !,
    gcd(X, -Y, Z).
gcd(X, 0, X) :- X > 0.
gcd(0, Y, Y) :- Y > 0.
gcd(X, Y, Z) :-
    X > Y, Y > 0,
    X1 is X - Y,
    gcd(Y, X1, Z).
gcd(X, Y, Z) :-
    X =< Y, X > 0,
    Y1 is Y - X,
    gcd(X, Y1, Z).

% For Q1a
parse_and_print :- process, !, parse_and_print.
parse_and_print.
process :- readline(L), exclude([X]>>(X =:= 32), L, LR), continue(LR).
readline(L) :- current_input(S), read_line_to_codes(S, L).
continue(end_of_file) :- nl, writeln("End"), !, fail.
continue(L) :- 
    complex_equation(E, L, []),
    writeln(E),
    eval(E, R),
    writeln(R).
continue(_) :- writeln("Invalid format").

% Part 1c
start(File1, File2):-
    open(File1, read, Str1),
    open(File2, write, Str2),
    process_file(Str1, Str2),
    close(Str1),
    close(Str2).

process_file(Str1, _) :-
    at_end_of_stream(Str1).

process_file(Str1, Str2) :-
    \+ at_end_of_stream(Str1),
    read_line_to_codes(Str1, L),
    exclude([X]>>(X =:= 32), L, LR),
    cont(Str2, LR),
    process_file(Str1, Str2).

cont(Str2, LR):-
    complex_equation(E, LR, []),
    eval(E, R),
    writeln(Str2, R).

cont(Str2, _):- writeln(Str2, "Input is not in valid format. See report or example file for valid input examples.").
