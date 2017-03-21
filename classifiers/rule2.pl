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
%Eduction= primary,Education=University,Education=Secondary,MS=Single,MS=married,MS=Divorced,Sex=Male,Sex=Female,Children=yes,Children=No,Approved=yes,Aproved=No
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
findall(F,(double_member([B,F],Binary,FeatureList),B=1),Features).

binary_to_features_with_class(Binary,Features):-
  FeatureList =['Eduction= primary','Education=University','Education=Secondary','MS=Single','MS=married','MS=Divorced','Sex=Male','Sex=Female','Children=yes','Children=no','class=yes','class=no'],
findall(F,(double_member([B,F],Binary,FeatureList),B=1),Features).

double_member(Items,List1,List2):-
  length(List1,L),
  between(1,L,I),
  nth1(I,List1,Ith),
  nth1(I,List2,Ith2),
  Items=[Ith,Ith2].

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
  Value #= NL*TP-FP*PL,
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




