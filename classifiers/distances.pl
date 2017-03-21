/*
 * Relational Instance-Based Learning with Lists and Terms
 * Horváth, T., Wrobel, S. & Bohnebeck, U. Machine Learning (2001) 43: 53. doi:10.1023/A:1007668716498
 *
 *Learing with kernels and logical representations . Probablistic
 inductive logic programming.
 * */

% can we get rid of the cuts? The clauses need to be muttally exclusive.
% Probbaly makes sense to sort the terms, but maybe a set differnce
% kernel.
:-use_module(library(clpfd)).

%two objects the same the distance is Zero.
o_o_dis(X,X,D):-D=0,!.
% two objects that are both atoms are constants and should use constant
% dis
o_o_dis(X,Y,D):-
    atomic(X),
    atomic(Y),
    o_o_constantDis(X,Y,D),!.
%two objects are compound terms and the functors are differnt.
o_o_dis(X,Y,D):-
    X =..[XF|XRest],
    length(XRest,XN),
    Y =.. [YF|YRest],
    length(YRest,YN),
    dif(XF,YF),
    o_o_functorDis(XF-XN,YF-YN,D),!.
%two objects, the functor name is the same but the lengths are differnt
o_o_dis(X,Y,D):-
    X =..[XF|XRest],
    length(XRest,XN),
    Y =.. [YF|YRest],
    length(YRest,YN),
    dif(XN,YN),
    o_o_functorDis(XF-XN,YF-YN,D),!.

% two objects are compound terms and the funtors are the same. (name and
% length) ?
o_o_dis(X,Y,D):-
    X=..[F|XRest],
    Y=..[F|YRest],
    length(XRest,N),
    length(YRest,N),
    maplist(o_o_dis,XRest,YRest,Distances),
    sumlist(Distances,D0),
    o_o_functorDis(F-N,F-N,D1),
    D is D1 +D0,!.
%Other cases
o_o_dis(X,Y,D):-
       D=0.


o_o_constantDis(X,Y,D):-
    dif(X,Y),
    D =1.
o_o_constantDis(X,X,D):-
    D=0.

o_o_functorDis(X,Y,D):-
    dif(X,Y),
    D=1.
o_o_functorDis(X,X,D):-
    D=0.


topkvote(T,Vote):-
    sort(T,ST),
    length(ST,L),
    L2 #= L div 2,
    length(First,L2),
    append(First,[Vote|_Rest],ST).

o_oclass_disClass(O,OClass,D).

%knn algorithm
data_instance_k_classification(D,I,K,C):-
    maplist(o_o_dis(I),D),
    keysort(D,DSorted),
    length(TopK,K),
    append(TopK,_,DSorted),
    topk_vote(TopK,C).
