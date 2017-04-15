/*
 * Relational Instance-Based Learning with Lists and Terms
 * Horváth, T., Wrobel, S. & Bohnebeck, U. Machine Learning (2001) 43: 53. doi:10.1023/A:1007668716498
 *
 *Learing with kernels and logical representations .
 *In : Probabilistic Inductive Logic Programming (Lecture Notes in Computer Science / Lecture Notes in Artificial Intelligence):
 *Theory and Applications Paperback – 14 Mar 2008
 *by Luc De Raedt (Editor)
 * */

% Can we get rid of the cuts? The clauses need to be mutually exclusive.
:-use_module(library(clpfd)).

%two objects that are the same the distance is Zero.
o_o_dis(X,X,D):-D #= 0,!.
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
    D #= D1 +D0,!.
%Other cases
o_o_dis(_X,_Y,D):-
       D #= 0.

o_o_constantDis(X,Y,D):-
    dif(X,Y),
    D #= 1.
o_o_constantDis(X,X,D):-
    D #= 0.

o_o_functorDis(X,Y,D):-
    dif(X,Y),
    D #= 1.
o_o_functorDis(X,X,D):-
    D #= 0.

%find the median vote
topk_vote(T,Vote):-
    pairs_values(T,V),
    msort(V,VSorted),
    length(T,L),
    L2 #= L div 2,
    length(First,L2),
    append(First,[Vote|_Rest],VSorted).

o_oclass_disClass(O,O2-Class,D-Class):-
   o_o_dis(O,O2,D).

key_value_keyvalue(Key,Value,Key-Value).

%knn algorithm:
%data is list of  pairs of terms and class
%e.g
%term(func(a,b))-pos
data_instance_k_classification(Data,I,K,C):-
    maplist(o_oclass_disClass(I),Data,DisAndClass),
    keysort(DisAndClass,DSorted),
    length(TopK,K),
    append(TopK,_,DSorted),
    %This is not a very good way of selecting k as you may have many values with the same distance, and the sorting just cuts these off
    %Dsorted = [1-pos,3-pos,3-pos,3-neg,3-neg]
    %Topk =[1-pos,3-pos,3-pos]
    topk_vote(TopK,C).


%test data
%This distance function only works with ground terms.
% Mis-match. Should probbaly have some kind of set kernel

data(X):-
    X=[household([people([
                      person(marge,female,38,blue,housekeeper),
                      person(homer,male,40,bald,nuclear_engineer),
                      person(bart, male,10,yellow,school_child),
                      person(lisa,female,8,yellow,school_child),
                      person(maggie,female,1,yellow,baby)]),
                pets([dog(santa_little_helper,greyhound)])])
               -pos,

       household([people([
                      person(liz,female,90,grey,queen),
                      person(phill,male,95,bald, prince),
                      person(charles,male,60,bald, waiter),
                      person(camila,female,60,grey,queen_to_be),
                      person(will,male, 30, bald, duke_nukem_player),
                      person(kate,female,30,brown, silent_witness),
                      person(harry,male,30,ginger, supermarket_opener)
                  ]),
                  pets([dog(monty,corgi), dog(willow,corgi),dog(holly,corgi)]),
                  guards([beefeaters, queens_guard]),
                  servants(uk_population)])
                  -neg,
       household([people([
                      person(peter,male,40, brown,fisherman),
                      person(lois,female,35,ginger,housekeeper),
                      person(chris,male,15, school_child),
                      person(meg,female,13, brown,school_child),
                      person(stewie,male,3, baby)]),
                  pets([
                      dog(brian, mongrel)])
                 ])
                 -pos,
       houshold([people([
                     person(fred,male,35,bronto_crane_driver),
                     person(wilma,female,28,house_keeper),
                     person(pebbles,female,2,baby)
                 ]),
                pets([dinosaur(dino,prosauropod),cat(baby_puss,saber_toothed_tiger)])])-
                pos,
       houshold([people([
                     person(tywin,male,66,bald,lord),
                     person(cersi,female,30,blond,schemer),
                     person(tyrion,male,28,politian),
                     person(jamie,male,32,kingslayer),
                     person(joffrey,16, blond,nice_guy),
                     person(myrcella,15, princess),
                     person(tommen,13,blond,wimp)]),
                 pets([]),
                 guards([the_mountain]),
                 servants([])

                ])-neg,
       houdhold([people([
                     person(darth_vader,55,bald, lord),
                     person(luke_skywalker,25, brown, jedi),
                     person(leia_skywalker,25,brown, princess)
                 ]),
                robots([r2d2,c3po])
                ])-neg,
       household([people([
                      person(rick_sanchez,200,blue,scientist),
                      person(beth_smith,30,blond,vet),
                      person(jerry_smith,35,brown,unemployed),
                      person(summer,15,ginger,school_child),
                      person(morty,11,brown,school_child)]),
                  servants(mr_meeseeks)
                 ])-pos

      ].


test(Test):-
    Test =
      household([people([
                         person(arthur, ginger,42,civil_servant),
                         person(molly,ginger,38,wizzard),
                         person(bill,ginger,15,school_child),
                         person(percy,ginger,14,school_child),
                         person(fred,ginger,14,school_child),
                         person(george,ginger,12,school_child),
                         person(ginny,ginger,10,school_child),
                         person(ron,ginger,11,school_child)]),
                     pets([
                         rat(scabbers),
                         owl(pigwidgeion),
                         owl(hermes),
                         owl(errol),
                         pygmy_puff(arnold)])]).
