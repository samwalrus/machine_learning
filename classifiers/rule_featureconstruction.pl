:-use_module('/media/sam/9bb6ab40-5f17-481e-aba8-7bd9e4e05d66/home/sam/Documents/Prolog_practise/logical_purity.pl').


%This file makes binary features from attributes that can take a number of values.
%You need either library reif or logical_purity
data(D):-
	D= [
	    [a1,a2],
	    [walrus,yellow],
	    [duck,red],
	    [egg,green],
	    [walrus,blue]].




attributes_data_features(As,D,AName-Pairs):-
	maplist(head_tail_list,As,_Tails,D),
	attributelist_features(As,[AName|Fs]),
	As =[Top|Rest],
	maplist(values_attributes_truth(Rest),Fs,Truths),
	maplist(i_i_p,Fs,Truths,Pairs).
attributes_data_features(As,D,AName-Pairs):-
        maplist(head_tail_list,_,Tails,D),
	attributes_data_features(As,Tails,_),
	attributelist_features(As,[AName|Fs]),
	As =[Top|Rest],
	maplist(values_attributes_truth(Rest),Fs,Truths),
	maplist(i_i_p,Fs,Truths,Pairs).

i_i_p(I1,I2,I1-I2).

attributelist_features(As,Fs):-
	setof(X, member(X,As),Fs).

values_attributes_truth(Vs,A,T):-
	maplist(=(A),Vs,T).

columdata_rowdata(C,R):-
	maplist(i_i_p,R,Cols,C).

head_tail_list(H,T,[H|T]).

%generalisation of double member/pair_list_list/3 used in other file
multimember(Ms,Lists):-
	maplist(head_tail_list,Ms,_,Lists).
multimember(Ms,Lists):-
	maplist(head_tail_list,_,Tails,Lists),
	multimember(Ms,Tails).


run(Fs,FeatureValuesPairs,FeatureNames,Rows,New):-
	data(D),
	findall(F,attributes_data_features(As,D,F),Fs),
	maplist(i_i_p,As,FeatureValuesPairs,Fs),
	lists_concatlist(FeatureValuesPairs,One),
	maplist(i_i_p,FeatureNames,FeatureValues,One),
	findall(Row,multimember(Row,FeatureValues),Rows),
	New =[FeatureNames|Rows].

%This gives a demo of how the code works
%?- run(Fs,FvPs,FNs,Rows,New), forall(member(M,New),format("~w~n",[M])).


%?-multimember(Ms,[[a,b,c],[1,2,3],[alpha,beta,gamma]]).

%?-multimember([a,1,alpha],Xs).


%Need to use library reif or logical purity
%?- data(D),attributes_data_features(As,D,F), write(F).

%?-data(D),attributes_data(As,D).
%@ D = [[a1, a2], [walrus, yellow], [duck, red], [egg, green], [walrus, blue]],
%@ As = [a1, walrus, duck, egg, walrus] ;
%@ D = [[a1, a2], [walrus, yellow], [duck, red], [egg, green], [walrus, blue]],
%@ As = [a2, yellow, red, green, blue] ;
				%@ false.

%@ D = [[walrus, yellow], [duck, red], [egg, green], [walrus, blue]],
%@ As = [walrus, duck, egg, walrus] ;
%@ D = [[walrus, yellow], [duck, red], [egg, green], [walrus, blue]],
%@ As = [yellow, red, green, blue] ;
				%@ false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%For attributes that have real number values%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(Features2):-
	data2(Data),
        %data2([Headers|Data]),
	fronts_lasts_lists(DataNoClass,Classes,Data),
	multimember(As,DataNoClass),%choice point here
        As = [Head|As1],
	maplist(make_int,As1,As2),
	writeln(As2),
        length(As1,L),
	numlist(1,L,Numlist),
	Classes =[_|Classes2],
	maplist(idnumber_class_attributevalue_struct,Numlist,Classes2,As2,Is),
	gen_feature(Is,Features),
	maplist(features_values_truths(Features),As2,Truths),
        maplist(head_pair_feature(Head),Features,Features2),
	writeln(Truths).

head_pair_feature(Z,X-Y,f(Z,X,Y)).

collumn_class(As-Classes):-
	data2([Headers|Data]),
	fronts_lasts_lists(DataNoClass,Classes,Data),
	multimember(As,DataNoClass). %choice point here

all_collumns(As2):-
	findall(As,collumn_class(As),As2).

thingc(As,Classes,Features,Truths):-
	maplist(make_int,As,As2),
	length(DataNoClass,L),
	numlist(1,L,Numlist),
	maplist(idnumber_class_attributevalue_struct,Numlist,Classes,As2,Is),
	gen_feature(Is,Features),
	maplist(features_values_truths(Features),As2,Truths).
%I need to put the attribute names in the features
thingd(Features,Truths,New):-
	thingb(As2),
	maplist(i_i_pair,As,Classes,As2),
	maplist(thingc,As,Classes,Features,Truths),
	lists_concatlist(Features,FsCat),
	findall(Zs,multimember(Zs,Truths),NewZs),
	maplist(lists_concatlist,NewZs,Concated),
	lists_concatlist([[FsCat],Concated],New).





i_i_pair(I,I2,I-I2).

idnumber_class_attributevalue_struct(I,C,A,i(I,C,A)).

data_pair(i(Id,X,Y),Y-(X-Id)).

front_last_list(Front,Last,List):-
	append(Front,[Last|[]],List).

fronts_lasts_lists(Fronts,Lasts,Lists):-
	maplist(front_last_list,Fronts,Lasts,Lists).
%?-fronts_lasts_lists(Fronts,Lasts,[[a,b,c],[1,2,3],[walrus,dog,cat]]).
%@ Fronts = [[a, b], [1, 2], [walrus, dog]],
%@ Lasts = [c, 3, cat] ;
%@ false.

%?-data2([Headers|Data]),fronts_lasts_lists(Attributes,Classes,Data).
%@ Headers = [a1, a2, class],
%@ Data = [[5, 6, pos], [7, 8, neg], [3, 5, pos], [7, 2, pos], [4, 4, pos], [4, 6, neg]],
%@ Attributes = [[5, 6], [7, 8], [3, 5], [7, 2], [4, 4], [4, 6]],
%@ Classes = [pos, neg, pos, pos, pos, neg]
%@ Unknown action:  (h for help)
				%@ Action? .

make_int(Number,NewNumber):-
	Temp is Number * 1000, %3 digits of accuracy for decimals
	round(Temp,Temp2),
	NewNumber is Temp2 *10. %number ends in 0 so that splits stay as ints.

data2(D):-
	D=
	 [[a1,a2,class],
	 [5,6,pos],
	 [7,8,neg],
	 [3,5,pos],
	 [7,2,pos],
	 [4,4,pos],
	 [4,6,neg]].

value_feature_truth(Value,R-Threshold,T):-
	Thing =.. [R,Value,Threshold,T],
	call(Thing).

features_values_truths(Fs,V,Ts):-
	maplist(value_feature_truth(V),Fs,Ts).


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




