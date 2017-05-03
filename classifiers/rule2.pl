:-use_module(library(clpb)).
:-use_module(library(clpfd)).

% A small example
pos(Ps):-
  Ps=[[0,1,0,1],
     [0,1,0,1],
     [1,0,0,1]].
neg(Ns):-
  Ns=[[1,0,1,0],
     [1,0,1,0],
     [0,1,1,0]].

%A larger example
database(D):-
%Eduction= primary,Education=University,Education=Secondary,MS=Single,MS=married,MS=Divorced,Sex=Male,Sex=Female,Children=yes,Children=No,Approved=yes,Approved=No
D=[
   [1,0,0,1,0,0,1,0,0,1,0,1],%1
   [1,0,0,1,0,0,1,0,1,0,0,1],%2
   [1,0,0,0,1,0,1,0,0,1,1,0],%3
   [0,1,0,0,0,1,0,1,0,1,1,0],%4
   [0,1,0,0,1,0,0,1,1,0,1,0],%5
   [0,0,1,1,0,0,1,0,0,1,0,1],%6
   [0,1,0,1,0,0,0,1,0,1,1,0],%7
   [0,0,1,0,0,1,0,1,0,1,1,0],%8
   [0,0,1,1,0,0,0,1,1,0,1,0],%9
   [0,0,1,0,1,0,1,0,1,0,1,0],%10
   [1,0,0,0,1,0,0,1,0,1,1,0],%11
   [0,0,1,0,0,1,1,0,1,0,0,1],%12
   [0,1,0,0,0,1,0,1,1,0,0,1],%13
   [0,0,1,0,0,1,1,0,0,1,1,0]%14
   ].

approved(As):-
  database(D),
  findall([F1,F2,F3,F4,F5,F6,F7,F8,F9,F10],(member([F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,C1,_],D),C1=1),As).

notapproved(NAs):-
  database(D),
  findall([F1,F2,F3,F4,F5,F6,F7,F8,F9,F10],(member([F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,C1,_],D),C1=0),NAs).

%To translate the binary vectors to the features on the larger example.
binary_to_features(Binary,Features):-
  FeatureList =['Eduction= primary','Education=University','Education=Secondary','MS=Single','MS=married','MS=Divorced','Sex=Male','Sex=Female','Children=yes','Children=no'],
findall(F,(pairmember_list_list(B-F,Binary,FeatureList),B=1),Features).

binary_to_features_with_class(Binary,Features):-
  FeatureList =['Eduction= primary','Education=University','Education=Secondary','MS=Single','MS=married','MS=Divorced','Sex=Male','Sex=Female','Children=yes','Children=no','class=yes','class=no'],
findall(F,(pairmember_list_list(B-F,Binary,FeatureList),B=1),Features).

pairmember_list_list(H1-H2,[H1|_T1],[H2|_T2]).
pairmember_list_list(H1-H2,[_|T1],[_|T2]):-
	pairmember_list_list(H1-H2,T1,T2).


%?-pairmember_list_list(X-X2,[1,2,3],[a,b,c]).
%@ X = 1,
%@ X2 = a ;
%@ X = 2,
%@ X2 = b ;
%@ X = 3,
%@ X2 = c ;
%@ false.

%To generate random examples. eg. ?- random_binary_matrix_x_y(9,20,Pos).
random_binary_vector_n(N,R):-
  length(R,N),maplist(random_between(0,1),R).

random_binary_matrix_x_y(X,Y,R):-
  length(R,Y),
  maplist(random_binary_vector_n(X),R).

%coverage checking predicates
covers_number(Features, Examples, Number):-
  maplist(my_covers_(Features), Examples, Numbers),
  sat(card([Number], Numbers)).

my_covers_(FeatureList,ExampleList,Covered):-
  build_structure(FeatureList,ExampleList,Structure),
  sat(Covered =:= *(Structure)).

build_structure([],[],[]).
build_structure([H1|T1],[H2|T2],[(H1=:=(H1*H2))|Structure]):-
  build_structure(T1,T2,Structure).

%exhaustive search method
start_exhaustive_search(Positives,Negatives,Features, Value,TP,FP):-
  length(Positives,PL),
  length(Negatives,NL),
  MaxL is max(PL,NL),
  member(Me,Positives),
  same_length(Features, Me),
  [TP,FP] ins 0..MaxL, % (in this case)
  Value #= NL*TP-FP*PL,
  labeling([max(Value)], [TP,FP]),
  covers_number(Features, Positives, TP),
  covers_number(Features, Negatives, FP),
  labeling(Features).

%Heuristic Search
specialise(Input,Output):-
  select(0,Input,1,Output).

search(NL,PL,Ps,Ns,Start,TP,FP,[r(Special,Value,cm(TP,FP))|Result]):-
  FP\==0, %Stop
  TP #>0,
  Value #>0,
  %what is this heuristic I am using
  heuristic_stats_value(unknown,pl_nl(PL-NL)-tp_fp(TP-FP),Value),
  labeling([max(Value)],[Value]),
  specialise(Start,Special),
  label([TP,FP]),
  covers_number(Special,Ps,TP),
  covers_number(Special,Ns,FP),
  TP1 in 0..TP,
  FP1 in 0..FP,
  search(NL,PL,Ps,Ns,Special,TP1,FP1,Result).
search(_,_,_,_,_,_,0,[]).

start_search(Ps,Ns,Result):-
  Ns=[Example|_],
  length(Example,Size),
  length(Ps,PL),
  length(Ns,NL),
  TP in 0..PL,
  FP in 0..NL,
  length(Start,Size),
  Start ins 0..0,
  search(NL,PL,Ps,Ns,Start,TP,FP,Result).

%How to deal with integers. Can I multiply by 1000 or 10000?
heuristic_stats_value(unknown,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
  Value #= NL*TP-FP*PL.

/*Elementry Heuristics */
heuristic_stats_value(unCovNeg,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value #= NL - FP. %True Negatives

%specificity is also known as true negative rate
heuristic_stats_value(specificity,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	TN #= NL-FP,
	Value is TN/NL.

%equivlant to recall and cov diff
heuristic_stats_value(covPos,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value = TP.

%recall aka sensitivity is equivlant to covpos for the purpose of ordering rules
heuristic_stats_value(recall,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value is TP/PL.

%also equivlant to recall and cov pos
heuristic_stats_value(support,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value is TP/(PL+NL).

/*End elementry heuristics */


heuristic_stats_value(cov_diff,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value #= TP -FP.

heuristic_stats_value(accuracy,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value is (Tp+(NL-FP))/(PL+NL).

heuristic_stats_value(coverage,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value is (TP+FP)/(PL+NL).

heuristic_stats_value(rate_diff,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value  is (TP/PL)-(FP/NL).

heuristic_stats_value(wra,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value is ((TP*FP)/(PL+NL))*((TP/(TP+FP))-(PL/(PL+NL))).

heuristic_stats_value(lincost,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	throw("Error lin cost not implemented yet-requires parameters").

heuristic_stats_value(rellincost,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	throw("Error rel lin cost not implemented yet-requires parameters").

heuristic_stats_value(precision,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	Value is TP/(TP+FP).

/*From Peters book not 100% sure this is correct*/
heuristic_stats_value(avg_recall,pl_nl(PL-NL)-tp_fp(TP-FP),Value):-
	TN #= NL-FP,
	%True negative rate is covered negatives /number of negatives 
	TNR is TN/NL,
	TPR is TP/PL,
	Value is (TPR +TNR)/2.


%?-approved(As),notapproved(NAs),start_search(As,NAs,Result).




