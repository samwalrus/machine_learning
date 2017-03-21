% This file has linear classifiers.
% The basic linear classifer and the perceptron
:-use_module(library(clpfd)).


linear_sep_data(Pos,Negs,All):-
	All=[
		[1.5998426,0.52985437,1],
                [0.25065517,1.30425162,1],
                [0.76148911,0.60419602,1],
                [0.75591032,-0.78994764,1],
                [1.63605539,0.9225655,1],
                [2.70520379,0.93285704,1],
                [1.82870703,2.34804646,1],
                [-0.08549264,0.99868399,1],
                [0.44906531,0.90555838,1],
                [0.49966187,1.59299327,1],
                [1.00003726,-0.13822094,1],
                [1.67943676,1.25283262,1],
                [-1.00158649,2.73839505,1],
                [3.32539035,-0.39289509,1],
                [2.17885898,0.05984356,1],
                [1.85977529,0.76782626,1],
                [1.34470454,0.18312675,1],
                [0.5974872,0.1228956,1],
                [-1.52394333,-1.24558361,-1],
                [-2.48452861,-1.91070328,-1],
                [-1.04605257,-2.55270759,-1],
                [1.02370408,-1.67944911,-1],
                [-0.80492117,-1.49215482,-1],
                [-1.64954319,-3.41635041,-1],
                [-2.35543276,-0.37750433,-1],
                [-0.32384031,-2.08235145,-1],
                [-1.56576954,-1.22018985,-1],
                [-1.27853841,-1.28469686,-1],
                [-1.97696119,0.23717806,-1],
                [-1.78965834,-1.09026084,-1]],
	findall(Instance,(member(InstanceC,All),append(Instance,[1|[]],InstanceC)),Pos),
	findall(Instance,(member(InstanceC,All),append(Instance,[-1|[]],InstanceC)),Negs).



multiply(X,Y,Z):-
	Z is X*Y.
%populate a list of Length with Pop.
populate_list(Pop,Length,ListPoped):-
	length(List,Length),findall(X,(member(X,List),X=Pop),ListPoped).

my_plus(X,Y,Z):-
	Z is X+Y.

my_subtract(X,Y,Z):-
	Z is X-Y.

%X and Y are vectors
vector_addition(X,Y,Z):-
	maplist(my_plus,X,Y,Z).


%X and Y are vectors
vector_subtraction(X,Y,Z):-
	maplist(my_subtract,X,Y,Z).

%S is a scalar and X is a vector.
scalar_multiplication(S,X,SX):-
	maplist(multiply(S),X,SX).

vector_length(V,L):-
	maplist(multiply,V,V,Squared),
	sumlist(Squared,SumSquare),
	L is SumSquare^0.5.

dot_product(V1,V2,Dot):-
	maplist(multiply,V1,V2,VMul),
	sumlist(VMul,Dot).

% Ps is a matrix,where each row is a pos example. Similar for Ns. W is
% vector and T is a scalar.
% To classifiy new example, take dotprouct with w and check if over T,
% if so classify as positive.
linear_classifier(Ps,Ns,classifier(W,T)):-
	Ps =[OneExample|_],
	length(OneExample,NumberOfFeatures),
	populate_list(0,NumberOfFeatures,Zerovector),
	length(Ps,NP),
	length(Ns,NN),
	foldl(vector_addition,Ps,Zerovector,SumvectorPos),
	foldl(vector_addition,Ns,Zerovector,SumvectorNegs),
	OneOverNp is 1/NP,
	OneOverNN is 1/NN,
        scalar_multiplication(OneOverNp,SumvectorPos,CenterOfMassP),
	scalar_multiplication(OneOverNN,SumvectorNegs,CenterOfMassN),
	vector_subtraction(CenterOfMassP,CenterOfMassN,W), %Subtracting P-N =W
	vector_length(CenterOfMassP,LengthP),
	vector_length(CenterOfMassN,LengthN),
	multiply(LengthP,LengthP,PSquared),
	multiply(LengthN,LengthN,NSquared),
	T is (PSquared - NSquared)/2.



perceptron(Pos,Neg,classifier(W,0)):-
	Pos =[OneExample|_],
	length(OneExample,NumberOfFeatures),
	populate_list(0,NumberOfFeatures,Zerovector),
	maplist(myappend([1]),Pos,PosClass),
	maplist(myappend([-1]),Neg,NegClass),
	append(PosClass,NegClass,Examples),
	convergedloop(Examples,Zerovector,W).

myappend(Y,X,Z):-
	append(X,Y,Z).


convergedloop(Examples,Inputvector,OutputVector):-
	innerforloop(Examples,Inputvector,UpdateVector),
	check_converged(Examples,Inputvector,UpdateVector,OutputVector).

check_converged(_Examples,W1,W2,W2):-W1=W2.
check_converged(Examples,W1,W2,W3):-
	dif(W1,W2),
	convergedloop(Examples,W2,W3).

innerforloop([],W,W).
innerforloop(ExamplesWithClass,W1,WFin):-
    ExamplesWithClass =[One|Rest],
    update_weight(One,W1,W2),
    innerforloop(Rest,W2,WFin).


update_weight_test(Example_with_class,W1,W2):-
	learning_rate(N),
	append(Example,[Class|[]],Example_with_class),
	Scalar is N *Class,
	scalar_multiplication(Scalar,Example,ExampleScaled),
	vector_addition(W1,ExampleScaled,W2).

misclassified(W,Example_with_class,TrueFalse):-
	append(Example,[Class|[]],Example_with_class),
	dot_product(Example,W,Dot),
	multiply(Dot,Class,Value),
	(   Value =<0->TrueFalse=true;TrueFalse=false).


learning_rate(0.1).

update_weight(Example_with_class,W1,W2):-
	misclassified(W1,Example_with_class,true),
	learning_rate(N),
	append(Example,[Class|[]],Example_with_class),
	Scalar is N *Class,
	scalar_multiplication(Scalar,Example,ExampleScaled),
	vector_addition(W1,ExampleScaled,W2).

update_weight(Example_with_class,W,W):-
	misclassified(W,Example_with_class,false).





linear_regression(Examples,Model).






