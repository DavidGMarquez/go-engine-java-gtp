notMember(_X,[]).
notMember(X,[Y|Ys]):- X\=Y,notMember(X,Ys).

eliminate(X,[X|Xs],Xs).
eliminate(X,[Y|Ys],[Y|Zs]):- eliminate(X,Ys,Zs).


addPairs([(I,J)|L],Lin,Lout) :- notMember((I,J),Lin),
	                        append(Lin,[(I,J)],LoutA),
				addPairs(L,LoutA,Lout).
addPairs(_,Lin,Lin).

numberOfElements([],0).
numberOfElements([_E|L],N) :- numberOfElements(L,X), N is X+1.

elementAt(1,X,[X|_]).
elementAt(I,X,[_|L]) :- I1 is I-1,elementAt(I1,X,L). 

% Lista de índices, lista de elementos, lista de elementos resultado
getElementAt([], _, []).
getElementAt([I|RI], L, [R|Resto]) :-
    elementAt(I, R, L),
    getElementAt(RI, L, Resto),!.

randomIndex(List,Random):-
	numberOfElements(List,N),
	random(X),
	N1 is X*N,
	ceiling(N1,Random).

randomElement(List,Element):-
	randomIndex(List,Random),
	elementAt(Random,Element,List),!.

maxList([A],A).
maxList([A|List],Max):-
 maxList(List,Max1),
 (A>=Max1, Max=A; A<Max1, Max=Max1).

find_indexes([],_,[]).
find_indexes(List,Value,Indexes) :-
	add_indexes(List,Value,[],Indexes,1).
add_indexes([],_,I,I,_).
add_indexes([A|List],Value,Indexes,IndexesOut,Current):-
	((A\=Value,append(Indexes,[],IndexesO));
	(A==Value,append(Indexes,[Current],IndexesO))),
	Next is Current+1,
	add_indexes(List,Value,IndexesO,IndexesOut,Next),!.

quick_sort(List,Sorted):- q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted) :-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).
pivoting(_H,[],[],[]).
pivoting(H,[X|T],[X|L],G):-X=<H,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-X>H,pivoting(H,T,L,G).
