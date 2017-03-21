:- use_module(library(clpb)).
:- use_module(library(clpfd)).

c_s_mining(Features, Value):-
    ExampleA = [0,1,0,1],
    ExampleB = [0,1,0,1],
    ExampleC = [1,0,0,1],

    ExampleD = [1,0,1,0],
    ExampleE = [1,0,1,0],
    ExampleQ = [0,1,1,0],

    same_length(Features, ExampleA),
    Positives = [ExampleA,ExampleB,ExampleC],
    Negatives = [ExampleD,ExampleE,ExampleQ],
    [TP,FP] ins 0..3, % (in this case)
    Value #= TP-FP,
    labeling([max(Value)], [TP,FP]),
    covers_number(Features, Positives, TP),
    covers_number(Features, Negatives, FP),
    labeling(Features).

covers_number(Features, Examples, Number):-
    maplist(covers_(Features), Examples, Numbers),
    sat(card([Number], Numbers)).

covers_([F1,F2,F3,F4], [E1,E2,E3,E4], Covered) :-
    sat(Covered =:= ((F1=:=E1)*(F2=:=E2)*(F3=:=E3)*(F4=:=E4))).
