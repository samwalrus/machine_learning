:-use_module(library(clpfd)).
:-use_module(reif).

/*
 * A shapelet is a subseq from the the set of instances. (In theory it
 * does not have to be, but how else would you define one?).
 *
 * The subsequence has a distance from all the sequences in the data
 * set.
 *
 * This is the point where the subsequence is closest to a given
 * sequence.
 *
 */





:- dynamic best/2.
/*
 *  log2(+Number, -Number)
 *This is a frog.
Returns the log2 value of a number, The shapelets paper uses loge but
most other sources would use log 2
*/
log2(X,Log2):-
	Log2 is log(X)/log(2).

%not sure if this is correct
loge(X,LogE):-
	LogE is log(X).


/*
 * Reified truth tests.
 *
 */
#=<(X,Y,Truth) :- X=<Y,Truth=true.
#=<(X,Y,Truth) :- X>Y, Truth=false.


#<( X,Y,Truth) :- X<Y,Truth=true.
#<( X,Y,Truth) :- X>=Y,Truth=false.


#>( X,Y,Truth) :- X>Y,Truth=true.
#>( X,Y,Truth) :- X=<Y,Truth=false.


#>=(X,Y,Truth) :- X>=Y,Truth=true.
#>=(X,Y,Truth) :- X<Y,Truth=false.


/*
 * truth count
 *
 */
tcount(P_1,Xs,N) :-
   N #>= 0,
   list_pred_tcount_(Xs,P_1,0,N).

list_pred_tcount_([]    , _ ,N ,N).
list_pred_tcount_([X|Xs],P_1,N0,N) :-
   if_(call(P_1,X), (N1 is N0+1, N1 #=< N), N1 = N0),
   list_pred_tcount_(Xs,P_1,N1,N).


/*
 * clpfd constrained length e.g:
 * ?- N#<3, fd_length(L,N).
 *
 */
fd_length(L, N) :-
   N #>= 0,
   fd_length(L, N, 0).

fd_length([], N, N0) :-
   N #= N0.
fd_length([_|L], N, N0) :-
   N1 is N0+1,
   N #>= N1,
   fd_length(L, N, N1).

/*
 * absoulte differnce between x and y
 */
abs(X,Y,Z):-
	Z is abs(X-Y).


/*
 *  abs distance between two sequences of numbers
 *
 *
 */
seq_seq_absdis(Seq1,Seq2,Dis):-
	same_length(Seq1,Seq2),
	maplist(abs,Seq1,Seq2,Dislist),
	sumlist(Dislist,Dis).

/*
exponetial(X,Y,Z):-
	Z is X^Y.

multiply(X,Y,Z):-
	Z is X*Y.

number_square(N,S):-
	S is N*N.

my_subtract(X,Y,Z):-
	Z is X-Y.

vector_subtraction(X,Y,Z):-
	maplist(my_subtract,X,Y,Z).

point_point_eudis(P1,P2,D):-
	vector_subtraction(P1,P2,Z),
	maplist(number_square,Z,Ss),
	sumlist(Ss,Sum),
	exponetial(Sum,0.5,D).

*/

/*
 * depth first search for subsequences
 *
 */
seq_subseq(List1,List2):-
	%prune(List2),
	append(List2,_,List1),
	dif(List2,[]).
seq_subseq([_|T],Subseq):-
	seq_subseq(T,Subseq).


/*
 * breadth first search for subsequences
 *
 */
bf_seq_subseq(Seq,Subseq):-
	fd_length(Seq,Max),
	Max2#=<Max,
	fd_length(Subseq,Max2),
	seq_subseq(Seq,Subseq).


/*
 * euclidian distance
 *
 *
 */
p_p_dis(P1,P2,Dis):-
	P1S is P1*P1,
	P2S is P2*P2,
	Sum is P1S +P2S,
	exponetial(Sum,0.5,Dis).



seq_seq_dis(Seq1,Seq2,Dis):-
	same_length(Seq1,Seq2),
	point_point_eudis(Seq1,Seq2,Dis).


% this can have pruning. Early abandon thing. abs or normal dis
smallseq_largeseq_dis(Sseq,Lseq,Dis):-
	findall(Subseq,(same_length(Subseq,Sseq),seq_subseq(Lseq,Subseq)),Subseqs),
        maplist(seq_seq_absdis(Sseq),Subseqs,Distances),
	aggregate(min(D),member(D,Distances),Dis).

/*
 *
 * early abandon distance between a small seq and a large seq
 * probbaly could be improved to use dcgs and push back lists instead of
 * assert retract
 *
 */

ea_smallseq_largeseq_dis(Sseq,Lseq,Subseq,Dis):-
	retractall(best(_,_)),
        assert(best(initial,10000)),
	findall(Subseq-Dis, ea_smallseq_largeseq_dis_h(Sseq,Lseq,10000,Subseq,Dis),Pairs),
        append(_,[Subseq-Dis|[]],Pairs).

ea_smallseq_largeseq_dis_h(Sseq,Lseq,BestSofar1,Subseq,Dis):-
	same_length(Sseq,Subseq),
	seq_subseq(Lseq,Subseq),
	best(_,BestSofar2),
	(   (   BestSofar2 < BestSofar1) ->
	    acumulate_dis(Sseq,Subseq,BestSofar2,Dis),
	    retractall(best(_,_)),
	    assert(best(Subseq,Dis))
	    ;(
	    acumulate_dis(Sseq,Subseq,BestSofar1,Dis),
	    retractall(best(_,_)),
	    assert(best(Subseq,Dis))
	    )

	).


acumulate_dis(Seq1,Seq2,Best,Dis):-
	acumulate_dis(Seq1,Seq2,Best,Dis,0).

acumulate_dis([],[],_Best,Dis,Dis).
acumulate_dis(Seq1,Seq2,Best,Dis,Ac):-
	Seq1=[H1|T1],
	Seq2=[H2|T2],
	abs(H1,H2,Dis1), %This is the distance function between 2 items in the sequence. Could be changed to a term distance.
	Ac1 is Dis1 + Ac,
	Ac1 <Best,
	acumulate_dis(T1,T2,Best,Dis,Ac1).



% refactor with assocs- think of what we need to do to control
% backtracks. %
%case1, move up the indexes by 1, from is the same.
s0_s1_seqs(S0-From,S1-From,AssocofSeqs):-
	assoc_to_keys(S0,S0Keys),
	maplist(plus(1),S0Keys,S1Keys),
	get_assoc(From,AssocofSeqs,FromAssoc),
	maplist(my_get_assoc(FromAssoc),S1Keys,S1Values), %this will fail if we go off the end
	maplist(x_y_pair,S1Keys,S1Values,S1Pairs),
	list_to_assoc(S1Pairs,S1),!.
	%S0 is  a pair, indexedassoc - seqfrom
	%S1 is similar
	%Assoc of seqs is an index assoc of assocs of the seqs and there indexes.
	%can use max_assoc(Assoc,Max,V) to get the size of the index,(when from 1 probbaly fast (use min as well if not from one).
%case2
%increase length of s
s0_s1_seqs(S0-From,S1-From,AssocofSeqs):-
	max_assoc(S0,MaxKey,_V),
	min_assoc(S0,MinKey,_V2),
	NewSize #=MaxKey-MinKey+2,
	NewMinKey #=1,
	my_num_list(NewMinKey,NewSize,S1Keys),
	get_assoc(From,AssocofSeqs,FromAssoc),
	maplist(my_get_assoc(FromAssoc),S1Keys,S1Values),
	maplist(x_y_pair,S1Keys,S1Values,S1Pair),
	list_to_assoc(S1Pair,S1),!.

%thirdcase
s0_s1_seqs(_S0-From,S1-From2,AssocofSeqs):-
	From2 #=From +1,
	get_assoc(From2,AssocofSeqs,FromAssoc),
	get_assoc(1,FromAssoc,Value),
	list_to_assoc([1-Value],S1).



newtest(Seqs):-
	Seqs =[[a,b,c],[x,y,z]],
	seqs_indexedassocs(Seqs,MainAssoc),
	get_assoc(1,MainAssoc,FromAssoc),
	get_assoc(1,FromAssoc,FirstValue),
	list_to_assoc([1-FirstValue],S0),

	format("S0 is ~w\n",[[1-FirstValue]]),

	s0_s1_seqs(S0-1,S1-A,MainAssoc),
	assoc_to_list(S1,S1L),
	format("S1 is ~w\n",[S1L]),

	s0_s1_seqs(S1-A,S2-B,MainAssoc),
	assoc_to_list(S2,S2L),
	format("S2 is ~w\n",[S2L]),

	s0_s1_seqs(S2-B,S3-C,MainAssoc),
	assoc_to_list(S3,S3L),
	format("S3 is ~w\n",[S3L]),

	s0_s1_seqs(S3-C,S4-D,MainAssoc),
	assoc_to_list(S4,S4L),
	format("S4 is ~w\n",[S4L]),

	s0_s1_seqs(S4-D,S5-E,MainAssoc),
	assoc_to_list(S5,S5L),
	format("S5 is ~w\n",[S5L]),

	s0_s1_seqs(S5-E,S6-F,MainAssoc),
	assoc_to_list(S6,S6L),
	format("S6 is ~w\n",[S6L]),

	s0_s1_seqs(S6-F,S7-G,MainAssoc),
	assoc_to_list(S7,S7L),
	format("S7 is ~w\n",[S7L]),

	s0_s1_seqs(S7-G,S8-H,MainAssoc),
	assoc_to_list(S8,S8L),
	format("S8 is ~w\n",[S8L]),

	s0_s1_seqs(S8-H,S9-I,MainAssoc),
	assoc_to_list(S9,S9L),
	format("S9 is ~w\n",[S9L]),

	s0_s1_seqs(S9-I,S10-J,MainAssoc),
	assoc_to_list(S10,S10L),
	format("S10 is ~w\n",[S10L]),

	s0_s1_seqs(S10-J,S11-K,MainAssoc),
	assoc_to_list(S11,S11L),
	format("S11 is ~w\n",[S11L]),

	s0_s1_seqs(S11-K,S12-L,MainAssoc),
	assoc_to_list(S12,S12L),
	format("S12 is ~w\n",[S12L]),

	s0_s1_seqs(S12-L,S13-_M,MainAssoc), %will fail as no more.
	assoc_to_list(S13,S13L),
	format("S13 is ~w\n",[S13L]).

%Seqs is a list of a list of seqs, N is how many you want.
newtest2(Seqs,N):-
	seqs_indexedassocs(Seqs,MainAssoc),
	get_assoc(1,MainAssoc,FromAssoc),
	get_assoc(1,FromAssoc,FirstValue),
	list_to_assoc([1-FirstValue],S0),
	mytest2h(S0-1,_SEnd-_ThingEnd,MainAssoc,N).


mytest2h(SBegin-Thing,SEnd-ThingEnd,Assocs,Count):-
	Count #>=1,
	assoc_to_list(SBegin,SBeginL),
	format("Seq is ~w\n",[SBeginL]),
	s0_s1_seqs(SBegin-Thing,SMiddle-Thing2,Assocs),
	Count2 #=Count-1,
	mytest2h(SMiddle-Thing2,SEnd-ThingEnd,Assocs,Count2).

mytest2h(SBegin-Thing,SEnd-Thing2,Assocs,Count):-
	Count #< 1,
	writeln('end'),
	s0_s1_seqs(SBegin-Thing,SEnd-Thing2,Assocs).

seqs_for_test(Seqs):-
	Seqs =[[a,b,c],[x,y,z]].

seqs_for_test2(Seqs):-
	Seqs =[[a,b,c,d,e,f],[q,w,e,r,t,y,z]].



my_get_assoc(Assoc,Key,Value):-
	get_assoc(Key,Assoc,Value).

% Seqs is a list of sequences, Assoc is an indexed assoc of indexed
% assocs
seqs_indexedassocs(Seqs,MainAssoc):-
       maplist(seq_indexedassoc,Seqs,SeqAssocs),
       length(SeqAssocs,Len),
       my_num_list(1,Len,Index),
       maplist(x_y_pair,Index,SeqAssocs,Pairs),
       list_to_assoc(Pairs,MainAssoc).

seq_indexedassoc(Seq,Assoc):-
       length(Seq,L),
       my_num_list(1,L,Index),
       maplist(x_y_pair,Index,Seq,Pairs),
       list_to_assoc(Pairs,Assoc).


%needs to be tested.
%Version with paramters, i.e min max to start with.
%case1, move up the indexes by 1, from is the same.
s0_s1_seqs_p(S0-From,S1-From,AssocofSeqs,_P):-
	assoc_to_keys(S0,S0Keys),
	maplist(plus(1),S0Keys,S1Keys),
	get_assoc(From,AssocofSeqs,FromAssoc),
	maplist(my_get_assoc(FromAssoc),S1Keys,S1Values), %this will fail if we go off the end
	maplist(x_y_pair,S1Keys,S1Values,S1Pairs),
	list_to_assoc(S1Pairs,S1),!.
	%S0 is  a pair, indexedassoc - seqfrom
	%S1 is similar
	%Assoc of seqs is an index assoc of assocs of the seqs and there indexes.
%can use max_assoc(Assoc,Max,V) to get the size of the index,(when from 1 probbaly fast (use min as well if not from one).
%case2
%increase length of s now up to a max size.
s0_s1_seqs_p(S0-From,S1-From,AssocofSeqs,P):-
	P = _Min-Max,
	max_assoc(S0,MaxKey,_V),
	min_assoc(S0,MinKey,_V2),
	NewSize #=MaxKey-MinKey+2,
	Max #>= NewSize,
	NewMinKey #=1,
	my_num_list(NewMinKey,NewSize,S1Keys),
	get_assoc(From,AssocofSeqs,FromAssoc),
	maplist(my_get_assoc(FromAssoc),S1Keys,S1Values),
	maplist(x_y_pair,S1Keys,S1Values,S1Pair),
	list_to_assoc(S1Pair,S1),!.

%thirdcase
%This is for starting on the next seq so min is important here
s0_s1_seqs_p(_S0-From,S1-From2,AssocofSeqs,P):-
	P= Min-_Max,
	From2 #=From +1,
	get_assoc(From2,AssocofSeqs,FromAssoc),
	my_num_list(1,Min,Indicies),
	maplist(my_get_assoc(FromAssoc),Indicies,Values),
	maplist(x_y_pair,Indicies,Values,Pairs),
	list_to_assoc(Pairs,S1).

%Seqs is a list of a list of seqs, N is how many you want. Then Min-Max
%?- seqs_for_test2(Seqs), newtest3(Seqs,15,2-4).

newtest3(Seqs,N,Min-Max):-
	seqs_indexedassocs(Seqs,MainAssoc),
	get_assoc(1,MainAssoc,FromAssoc),
	my_num_list(1,Min,Indicies),
	maplist(my_get_assoc(FromAssoc),Indicies,Values),
	maplist(x_y_pair,Indicies,Values,Pairs),
	list_to_assoc(Pairs,S0),
	mytest3h(S0-1,_SEnd-_ThingEnd,MainAssoc,N,Min-Max).


mytest3h(SBegin-Thing,SEnd-ThingEnd,Assocs,Count,Min-Max):-
	Count #>=1,
	assoc_to_list(SBegin,SBeginL),
	format("Seq is ~w\n",[SBeginL]),
	s0_s1_seqs_p(SBegin-Thing,SMiddle-Thing2,Assocs,Min-Max),
	Count2 #=Count-1,
	mytest3h(SMiddle-Thing2,SEnd-ThingEnd,Assocs,Count2,Min-Max).

mytest3h(SBegin-Thing,SEnd-Thing2,Assocs,Count,Min-Max):-
	Count #< 1,
	writeln('end'),
	s0_s1_seqs_p(SBegin-Thing,SEnd-Thing2,Assocs,Min-Max).


%for a feature find its info gain and set to the best
% Next check each other feature inturn, if infogain calc is abandoned
% move on to next feature.
% If info gain suceeds set best feature and info gain to that and carry
% on.
%
%

e_test_data(Instances):-
	Instances=
	    [i(1,neg),
	     i(2,pos),
	     i(3,neg),
	     i(4,pos),
	     i(5,neg),
	     i(6,pos),
	     i(7,neg),
	     i(8,pos),
	     i(9,neg),
	     i(10,neg),
	     i(11,neg),
	     i(12,neg)].


i(1,neg,-5).
i(2,pos,-3.1).
i(3,neg,-2.7).
i(4,pos,7.1).
i(5,neg,0).
i(6,pos,8.5).
i(7,neg,7).
i(8,pos,9.0).
i(9,neg,9.0).
i(10,neg,13.7).
i(11,neg,15.1).
i(12,neg,20.1).

data_pair(i(Id,X,Y),Y-(X-Id)).


/*
 * Main Program
 *
 * This learns a decision stump shapelet classifier.
 * Data is a list of instances i(id,class). It should be orderd as
 * alternatives, ie. pos, neg, pos, neg.
 */
% Shapelet feature is the subsequence the relation and splitting value
% that best classifies data, Min-Max is the min and maxium length of
% a shapelet
% p is for parameter ie min-max
data_p_shapeletfeature(DataList,Min-Max,ShapeletFeature):-
	%From data generate a first potenital shaplet
	%First subseq from data
	data_min_firstshapelet(DataList-DataAssoc,Min,PotShapeletSeq),
	%length(PotShapeletSeq,LP),%This is an assoc not a list, length will be min anyway.
        %Max #> LP,
	%calculate the distance from this shapelet to all sequences in data or is this done in the next predicate?
	%Make the feature ie operator and threshold that best splits the data
	%set this shapelet, the operator and threshold in best.
	data_p_best_potshape_shapeletfeature(DataList-DataAssoc,Min-Max,best(empty,_Threshold-_Op,1000),PotShapeletSeq,ShapeletFeature).

data_min_firstshapelet(D-MainAssoc,Min,S0-1):-
	seqs_indexedassocs(D,MainAssoc),
	get_assoc(1,MainAssoc,FromAssoc),
	my_num_list(1,Min,Indicies),
	maplist(my_get_assoc(FromAssoc),Indicies,Values),
	maplist(x_y_pair,Indicies,Values,Pairs),
	list_to_assoc(Pairs,S0).

% go through all the potential shaplets searching for the one that gives
% best infogain. Can best and pot be the same thing.
data_p_best_potshapelet_shapeletfeature(DList-DAssoc,Min-Max,Best,P,S):-
	data_best_sofar_potshapelet_entropy(DList,Best,[],P,Entropy),
	s0_s1_seqs_p(P,P2,DAssoc,Min-Max),
	data_p_best_potshapelet_shapeletfeature(DList-DAssoc,Min-Max,Best2,P2,S).
% If you can not gen a new subsequence then we have reached then end
% return best.
data_p_best_potshape_shapeletfeature(_DL-_DA,_Min-_Max,Best,_PotShape,Best).


/*
 * Data is a list of instances i(id,class). It should be orderd as
 * alternatives, ie. pos, neg, pos, neg.
 *
 *
 * When called to start the learning sofar should be bound to the empty
 * list. In the recursive calls it contains the instances that have been
 * looked at.
 *
 *
 */

%need to think about stopping cases.
%Best should be a tripple, best(Shapelet,Feature,Entropy).
%If pos or neg count reach zero then stop
%The first time we take two, then we take one.
data_best_sofar_potshapelet_entropy(Data,Best,Sofar,Shapelet,Entropy):-
	Sofar =[],
	Data =[i(I_id1,I1_class),i(I_id2,I2_class)|Rest],
	%get distances of 2 instances from Shapelet
	i(I_id1,_,Dis1),
	i(I_id2,_,Dis2),
        %add the two instances and these distances to the sofar seen list
	append(Sofar,[i(I_id1,I1_class,Dis1),i(I_id2,I2_class,Dis2)],SoFar2),
	%how many positives and negitives have we not yet seen.
	%maybe this could be improved by just removing one from a count down value.
	aggregate_all(count,member(i(_,pos),Rest),Poscount),
	aggregate_all(count,member(i(_,neg),Rest),NegCount),
	RestSize =Poscount-NegCount,
	%From the sofar list generate possible splitting points as features
	gen_feature(SoFar2,Fs),
	%For all possible splitting points, find the two splits
	maplist(data_relationvalue_split1_split2(SoFar2),Fs,Split1,Split2),
	%find the optimisic entropy for the pairs of splits
	maplist(optimistic_entropy(RestSize),Split1,Split2,Entropies),
	%what F does come from
	maplist(x_y_pair,Entropies,Fs,Entropies_Fs),
	%take the smallest value feature
	keysort(Entropies_Fs,[ValueCan-FCan|_]),
	best0_candidatebest_best1(Best,can(Shapelet,FCan,ValueCan),Best1),
        %if this is better continue calculating entropy to find out its true value.
	%If the poscount or neg count is zero then optimistic entropy the correct entropy and you return this entropy and feature for that shapelet
        data_counts_best_sofar_feature_entropy_h(Rest,Counts,Best1,SoFar2,Shapelet,Entropy).
data_counts_best_sofar_shapelet_entropy(Data,Counts0,Best,Sofar,Shapelet,Entropy):-
	dif(Data,[]),
	Counts0 = CountPos0-CountNeg0,
	maplist(#>,[CountPos0,CountNeg0],[0,0]),
	Data =[i(I_id1,I1_class)|Rest],
	%get distances of 1 instances from Shapelet
	i(I_id1,_,Dis1),
	%add the this instance and its distances to the sofar seen list
	append(Sofar,[i(I_id1,I1_class,Dis1)],SoFar2),
	%how many positives and negitives have we not yet seen.
	%maybe this could be improved by just removing one from a count down value.
	instanceclass_counts0_counts1(I1_class,Counts0,Counts1),
	%From the sofar list generate possible splitting points as features
	gen_feature(SoFar2,Fs),
	%For all possible splitting points, find the two splits
	maplist(data_relationvalue_split1_split2(SoFar2),Fs,Split1,Split2),
	%find the optimisic entropy for the pairs of splits
	maplist(optimistic_entropy(Counts1),Split1,Split2,Entropies),
	%lit of entropies
	%selectmininfogain
	min_list(Entropies,Infogain1),
	Infogain1 < Best, %if it is less then carry on???
	%Best needs to have the feature as well as its entropy.
	data_counts_best_sofar_feature_entropy(Rest,Counts1,Best,SoFar2,Shapelet,Entropy).%rest might need the second removed element as the head

%Steadfast improvement? Constraints to improve back tracking
data_counts_best_sofar_feature_entropy_h(Data,0-Other,Best1,_,Best1,Entropy):-
	%pos count is 0
	dif(Data,[]),
	dif(Other,0),
	simple_entropy.
data_counts_best_sofar_feature_entropy_h(Data,Other-0,Best1,_,Best1,Entropy):-
	%neg count is 0
	dif(Data,[]),
	dif(Other,0),
	simple_entropy.
data_counts_best_sofar_shapelet_entropy_h([],O1-O2,_Best,Sofar,_Shapelet,Entropy):-
	%probably redudant but if whole list is zero
	dif(O1,0),
	dif(O2,0),
	simple_entropy(Sofar,[],_Shapelet,Entropy).



instanceclass_counts0_counts1(Class,Counts0,Counts1):-
	Class =pos,
	Counts0 = PosCount0-NegCount,
	PosCount1 #= PosCount0 -1,
	Counts1= PosCount1-NegCount.
instanceclass_counts0_counts1(Class,Counts0,Counts1):-
	Class = neg,
	Counts0 = PosCount-NegCount0,
	NegCount1 #=NegCount0 -1,
	Counts1 = PosCount -NegCount1.


best0_candidatebest_best1(best(_Shapelet,_FBest0,Value),can(ShapeletCan,FCan,ValueCan),best(S,F,V)):-
	Value #< ValueCan,
	S =ShapeletCan,
	F =FCan,
	V= ValueCan.
best0_candidatebest_best1(best(Shapelet,FBest0,Value),can(_ShapeletCan,_FCan,ValueCan),best(S,F,V)):-
	Value #>= ValueCan,
	S = Shapelet,
	F = FBest0,
	V = Value.




data_relationvalue_split1_split2(Is,Relation-Value,Ts,Fs):-
	P_2 =..[Relation,Value],
	tpartition(P_2,Is,Ts,Fs).


test:-
	findall(i(Id,Class,Dis),i(Id,Class,Dis),Is),
	gen_feature(Is,Features),
	maplist(data_relationvalue_split1_split2(Is),Features,Split1,Split2),
	format("~n~n~w~n~n~n~w~n~n~w~n~n~w",[Features,Is,Split1,Split2]).


%example from the paper:
% [-6/10]log2(6/10)-(4/10)log(4/10)]-[(4/10)[-4/4)log2(4/4)]+(6/10)[-(4/6)log2(4/6)-(2/6)log2(2/6)]]=
% 0.291
%
% From Peters book we want the weighted average of entropies.
% We want to choose the feature with lowest entopy.
% Or highest information gain.
% I think information gain is the entropy before the split, then minus
% the entropy after the split

simple_entropy(PosCount1,_,Entropy):-
	PosCount1 = 0,
	Entropy = 0.

simple_entropy(_,NegCount1,Entropy):-
	NegCount1 = 0,
	Entropy = 0.

simple_entropy(Poscount,Negcount,Entropy):-
	ProportionPos is Poscount/(Poscount+Negcount),
	ProportionNeg is 1-ProportionPos,
	loge(ProportionPos,LogPropPos),
	loge(ProportionNeg,LogPropNeg),
	Entropy is -ProportionPos*LogPropPos-ProportionNeg*LogPropNeg.

/*
 * Rest size if a pair poscount-negcount
 * There will be two entropy scores.
 * Either all positives on left and negs on right
 * or vice versa
 *
 *
 *
 * Find the entropy of split1 (Two possibllites, with added pos or added
 * negs
   Find the entropy of split2 (Two possibilites, with added pos or added
   neg
 * * Take the weighted average of these. * * *
 *
 * I think the paper uses base e rather than base 2 for some reason
 *
 * aggregate count is not correct
 */
optimistic_entropy(RestSize,Split1,Split2,BestEntropy):-
	RestSize =Poscount-NegCount,
	length(Split1,L1),
	length(Split2,L2),
	Total is L1 +L2+Poscount+NegCount,
	aggregate_all(count,member(i(Id,pos,_),Split1),PosSplit1),
        aggregate_all(count,member(i(Id,pos,_),Split2),PosSplit2),
	NegSplit1 is L1 -PosSplit1,
	NegSplit2 is L2 -PosSplit2,
        OptimisticPosCount1 is Poscount + PosSplit1, %pos added to left split
	OptimisticNegCount1 is NegCount + NegSplit1, %Neg added to left split
	OptimisticPosCount2 is Poscount + PosSplit2, %pos added to right split
	OptimisticNegCount2 is NegCount + NegSplit2, %neg added to right split
	%Case 1:
	simple_entropy(OptimisticPosCount1,NegSplit1,E1), %In split 1 you have the pos from split1 +the added pos, and the negs from split1
	simple_entropy(PosSplit2,OptimisticNegCount2,E2),% In split 2 you have the pos from split2 and the negs of split2 + the added negs
	%now work out the weighted average of these EX
	Case1Split1Size is OptimisticPosCount1+NegSplit1,
	Case1Split2Size is PosSplit2+OptimisticNegCount2,
	Case1WeightedEntropy is ((Case1Split1Size/Total)*E1)+((Case1Split2Size/Total)*E2),
	%Case 2:
	simple_entropy(PosSplit1,OptimisticNegCount1,E3), %In split 1 you have the pos from split1, and the negs of splits +added negs
	simple_entropy(OptimisticPosCount2,NegSplit2,E4), %In split2 you have the pos from split2 +the added pos and the negs of split2
	%now work out the weighted average of these EY
	Case2Split1Size is PosSplit1+OptimisticNegCount1,
	Case2Split2Size is OptimisticPosCount2+NegSplit2,
	Case2WeightedEntropy is ((Case2Split1Size/Total)*E3)+((Case2Split2Size/Total)*E4),
	%
	x_y_lower(Case1WeightedEntropy,Case2WeightedEntropy,Lower),%Take the lower value for entropy as best value
	BestEntropy = Lower.

%I think this is the same as min
x_y_lower(X,Y,Y):-
	X>=Y.
x_y_lower(X,Y,X):-
	Y>X.

%test for optimistic infogain.
%
% The paper uses log base e .
% info gain vs entropy . No read need to
test_optimistic(X):-
	optimistic_entropy(3-2,[i(1,pos,_)],[i(2,neg,_),i(3,pos,_),i(4,neg,_),i(5,pos,_)],X).
%
%Test to get correct answer as paper X3.
%entropy of whole set, - entropy of split
%?-simple_entropy(6,4,X1),test_optimistic(X2),X3 is X1-X2.


% to find optimal split point, we check only mean values of each pair of
% adjacent points
%
% First order values.
unsorted_sorted(I,Sorted):-
	maplist(data_pair,I,P),
	keysort(P,SP),
	maplist(data_pair,Sorted,SP).
%find pairs in the sorted list where the class is differnt.
pair_member(X,Y,[X,Y|_]):-
	X=i(_,C1,_),
	Y=i(_,C2,_),
	dif(C1,C2).
pair_member(X,Y,[_|Rest]):-
	X=i(_,C1,_),
	Y=i(_,C2,_),
	dif(C1,C2),
        pair_member(X,Y,Rest).


relation(i(_,pos,_),#<).
relation(i(_,neg,_),#>=).

avg(i(_,_,V1)-i(_,_,V2),Avg):-
	Avg is (V1+V2)/2.

pair_relation(I1-I2,Relation):-
	unsorted_sorted([I1,I2],[L1,L2]),
	relation(L1,Relation).

x_y_pair(X,Y,X-Y).

gen_feature(I,F):-
	unsorted_sorted(I,Is),
	findall(X-Y,pair_member(X,Y,Is),Pairs),
	maplist(avg,Pairs,Avgs),
	maplist(pair_relation,Pairs,Relations),
	maplist(x_y_pair,Relations,Avgs,F).




%split data into d1 and d2 using feature and feature value
%for real number.
data_feature_fvalue_d1_d2(Data,Feature,fvalue,D1,D2).




data_tree(Data,Tree).

tree_data_metrics(Data,Tree,Metrics).


plus1(X,Y):-
	Y #= X+1.


my_num_list(Min,Max,List):-
	Length #= Max - Min+1,
	length(List,Length),
	chain(List, #<),
	List =[Min|_Rest],
	last(List,Max).














