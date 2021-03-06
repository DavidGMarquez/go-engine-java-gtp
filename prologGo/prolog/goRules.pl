:- consult('listOperations.pl').

% Controla el turno
:- dynamic hasPlayerWithTheMove/3.
% Controla el n�mero de movimientos que hay.
:- dynamic hasNumberMoves/3.
% Comprueba si esta en el tablero y en que posicion.
:- dynamic isInSquare/4.

:- dynamic hasNumberOfCapturedStones/3.

position(0).

% Establece la configuracion por defecto del tablero 
:- dynamic initialConfiguration/1.

stone(white(X)) :- integer(X), X>=0.
stone(black(X)) :- integer(X), X>=0.

% Definimos las distintas lineas que puede tener el tablero esto es para dimensionar todo el tablero
% Las casillas se cuentan de izquierda a derecha y de abajo arriba
line(1).
line(2).
line(3).
line(4).
line(5).
line(6).
line(7).
line(8).
line(9).
line(10).
line(11).
line(12).
line(13).
line(14).
line(15).
line(16).
line(17).
line(18).
line(19).

isFile(1,a).
isFile(2,b).
isFile(3,c).
isFile(4,d).
isFile(5,e).
isFile(6,f).
isFile(7,g).
isFile(8,h).
isFile(9,j).
isFile(10,k).
isFile(11,l).
isFile(12,m).
isFile(13,n).
isFile(14,o).
isFile(15,p).
isFile(16,q).
isFile(17,r).
isFile(18,s).
isFile(19,t).


% Grid sizes Los distintos tipos de tablero que se pueden elegir
hasSquareSize(0,9).
hasSquareSize(1,13).
hasSquareSize(2,19).

% Comprueba si una linea esta en el tablero
lineInGrid(X) :-  line(X), initialConfiguration(C),hasSquareSize(C,S),X=<S.

% Para saber si esa casilla esta en el borde del tablero de arriba
isUEdge(X) :- line(X), initialConfiguration(C), hasSquareSize(C,X).
% Como si es 1 siempre sera el limite inferior del tablero la casilla
isLEdge(1).


% Define el color de la piedra
hasColor(white(_X),white).
hasColor(black(_X),black).
isOppositeColor(black,white).
isOppositeColor(white,black).

%Define el numero de la piedra (todas las piedras estan numeradas secuencialmente)
hasNumber(black(X),X).
hasNumber(white(X),X).

% Checks whether two stones are adyacent, regardless of their color
isAdyacent(Position,Stone1,Stone2) :-  isInSquare(Position,Stone1,I1,J1),
	                               isInSquare(Position,Stone2,I2,J2),
				       ((I1=:=I2,abs(J1-J2)=:=1);J1=:=J2,abs(I1-I2)=:=1).

% Checks whether two adyacent stones have the same color
isAdyacentSame(Position,Stone1,Stone2) :- isAdyacent(Position,Stone1,Stone2),
	                                  hasColor(Stone1,Color), hasColor(Stone2,Color).

% Checks whether two adyacent stones DON'T have the same color
isAdyacentOpposite(Position,Stone1,Stone2) :- isAdyacent(Position,Stone1,Stone2),
	                                      not(isAdyacentSame(Position,Stone1,Stone2)).

% Checks whether the (I,J) position is an empty square adyacent to Stone
isAdyacentEmpty(Position,Stone,I,J) :- isInSquare(Position,Stone,X,Y),
				       lineInGrid(I),lineInGrid(J),
				       not(isInSquare(Position,_,I,J)),
				       ((I=:=X,abs(J-Y)=:=1);J=:=Y,abs(I-X)=:=1).

% Returns all of the (I,J) pairs of empty squares adyacent to Stone
listAdyacentEmpty(Position,Stone,L) :-  findall((I,J),isAdyacentEmpty(Position,Stone,I,J),L).

% Checks whether a stone is surrounded by other stones,
% regardless of their color (liberties)
isSurrounded(Position,Stone) :- isInSquare(Position,Stone,I,J),
				((IU is I+1,isInSquare(Position,_Stone1,IU,J));isUEdge(I)),
				((ID is I-1,isInSquare(Position,_Stone2,ID,J));isLEdge(I)),
                                ((JL is J-1,isInSquare(Position,_Stone3,I,JL));isLEdge(J)),
				((JR is J+1,isInSquare(Position,_Stone4,I,JR));isUEdge(J)).

% Legal moves.
% 0,0 is equivalent to passing
% TODO: suicide, ko
passingMove((0,0)).
legalMove(_,_,0,0).
legalMove(Position, _Color, I, J) :- lineInGrid(I), lineInGrid(J),
	                            not(isInSquare(Position,_S,I,J)).

legalMoves(Position,Color,L) :- findall((I,J),legalMove(Position,Color,I,J),L).
						

% Checks whether there is a chain of connected stones
% Starting at position I,J, returns non-duplicate results in Lout list.
listConnected(P,Color,I,J,Lin,Lout) :- isInSquare(P,Stone1,I,J), hasColor(Stone1,Color),
	                               notMember(Stone1,Lin), append(Lin,[Stone1],LauxA),
				       I1 is I-1, listConnected(P,Color,I1,J,LauxA,LauxB),
				       J1 is J-1, listConnected(P,Color,I,J1,LauxB,LauxC), 
				       I2 is I+1, listConnected(P,Color,I2,J,LauxC,LauxD), 
				       J2 is J+1, listConnected(P,Color,I,J2,LauxD,Lout),!.
listConnected(_P,_Color,_I,_J,Lin,Lin).

% Returns a list of stones connected to Stone, including Stone
connectedToStone(P,Stone,L) :- isInSquare(P,Stone,I,J),
			       hasColor(Stone,Color),
			       listConnected(P,Color,I,J,[],L).

% Checks whether L, containing S, is a chain of stones
% in the "captured" state
listSurrounded(P,Stone,L) :- connectedToStone(P,Stone,L),
	                      allSurrounded(P,L).
allSurrounded(P,[Stone|[]]) :- isSurrounded(P,Stone).
allSurrounded(P,[Stone|L]) :- isSurrounded(P,Stone),allSurrounded(P,L).

listLiberties(P,[Stone|L],Lin,Lout) :- listAdyacentEmpty(P,Stone,Le),
	                               addPairs(Le,Lin,LoutA),
				        listLiberties(P,L,LoutA,Lout),!.

 
listLiberties(_P,_,Lin,Lin).


% Game actions
placeStone(P,Color,I,J) :- legalMove(P,Color,I,J),
	                   hasNumberMoves(P,Color,X),
			   N is X+1,
			   retractall(hasNumberMoves(P,Color,X)),
			   assert(hasNumberMoves(P,Color,N)),
			   hasPlayerWithTheMove(P,Color),
			   hasColor(Stone,Color),
			   hasNumber(Stone,N),
			   assert(isInSquare(P,Stone,I,J)),
			   checkCaptured(P,Stone),
			   retractall(hasPlayerWithTheMove(P,Color)),
			   isOppositeColor(Color,OtherColor),
			   assert(hasPlayerWithTheMove(P,OtherColor)),!.

% L is the letter representing the File (a-t)
placeStone(P,Color,I,L) :- isFile(J,L),placeStone(P,Color,I,J),!.

% Checks if the stone has captured any stones around it
checkCaptured(P,Stone) :- isAdyacentOpposite(P,Stone,Stone2),
	                  listSurrounded(P,Stone2,L),
			  removeStones(P,L).
% Returns true so as to not interrupt the stone placing process
checkCaptured(_,_).

removeStones(_,[]).
removeStones(Position,[Stone|List]) :- retractall(isInSquare(Position,Stone,_,_)),
				       addPoints(Position,Stone),
				       removeStones(Position,List).

addPoints(Position,Stone) :- hasColor(Stone,Color),
	hasNumberOfCapturedStones(Position,Color2,X),
	isOppositeColor(Color,Color2),
	N is X+1,
	retractall(hasNumberOfCapturedStones(Position,Color2,X)),
	assert(hasNumberOfCapturedStones(Position,Color2,N)).
	
% Evaluates a possible move in board 0
evaluateVirtualTablet(P,PV,Color,I,J,Points):-
	copyBoard(P,PV),
	assert(hasPlayerWithTheMove(PV,Color)),
	placeStone(PV,Color,I,J),
	hasNumberOfCapturedStones(PV,Color,Points),
	cleanBoard(PV),!.


% Evaluates a move by assigning it a score
% The score is the number of opposite stones that will be captured.
evaluateMove(P,Color,(I,J),N) :- hasNumberOfCapturedStones(P,Color,C),
	                         evaluateVirtualTablet(P,eval,Color,I,J,C2),
			         N is C2-C.

positiveMove(P,Color,(I,J),N):- evaluateMove(P,Color,(I,J),N),N>0.
neutralMove(P,Color,(I,J)):- evaluateMove(P,Color,(I,J),0),not(passingMove((I,J))).
		

% Returns in a list all of the moves that will capture opposing stones
positiveMoves(_,_,[],L,L,P,P).
positiveMoves(P,Color,[Move|Allmoves],PositiveMoves,PositiveMovesOut,PointsIn,PointsOut) :-
	((positiveMove(P,Color,Move,N),append(PositiveMoves,[Move],PositiveMovesO),append(PointsIn,[N],PO));
	(not(positiveMove(P,Color,Move,_)),append(PositiveMoves,[],PositiveMovesO),append(PointsIn,[],PO))),
	positiveMoves(P,Color,Allmoves,PositiveMovesO,PositiveMovesOut,PO,PointsOut),!.

neutralMoves(_,_,[],L,L).
neutralMoves(P,Color,[Move|Allmoves],NeutralMoves,NeutralMovesOut) :-
	((neutralMove(P,Color,Move),append(NeutralMoves,[Move],NeutralMovesO));
	(not(neutralMove(P,Color,Move)),append(NeutralMoves,[],NeutralMovesO))),
	neutralMoves(P,Color,Allmoves,NeutralMovesO,NeutralMovesOut),!.


% Picks any neutral move from the list of legal moves
anyMove(P,Color,Move) :-
	legalMoves(P,Color,L),
	neutralMoves(P,Color,L,[],NM),
	randomElement(NM,Move).
anyMoves(P,Color,Moves) :-
        legalMoves(P,Color,L),
	neutralMoves(P,Color,L,[],Moves).

% Picks any positive move from the list of legal moves
anyPositiveMove(P,Color,Move) :-
	legalMoves(P,Color,L),
	positiveMoves(P,Color,L,[],PM,[],_),
	randomElement(PM,Move).

% TO-DO best positive move
bestMove(P,Color,Move) :-
	legalMoves(P,Color,L),
	positiveMoves(P,Color,L,[],PM,[],Points),
	maxList(Points,Max),
	find_indexes(Points,Max,In),
	getElementAt(In,PM,Moves),
	randomElement(Moves,Move).

bestMoves(P,Color,Moves):-
	legalMoves(P,Color,L),
	positiveMoves(P,Color,L,[],PM,[],Points),
	maxList(Points,Max),
	find_indexes(Points,Max,In),
	getElementAt(In,PM,Moves).

chooseMove(P,Color,X,Y) :- bestMove(P,Color,(X,Y)),!.
chooseMove(P,Color,X,Y) :- anyMove(P,Color,(X,Y)),!.
chooseMove(_,_,0,0).

% returns a list of moves
chooseMoves(P,Color,L):- bestMoves(P,Color,L),!.
chooseMoves(P,Color,L):- anyMoves(P,Color,L).
	   
play(I,J) :- placeStone(0,black,I,J),
	     chooseMove(0,white,X,Y),
	     placeStone(0,white,X,Y).

cleanBoard(C) :- retractall(isInSquare(C,_,_,_)),
	         retractall(hasNumberMoves(C,_,_)),
		 retractall(hasNumberOfCapturedStones(C,_,_)),
		 assert(hasNumberMoves(C,black,0)),
		 assert(hasNumberMoves(C,white,0)),
		 assert(hasNumberOfCapturedStones(C,black,0)),
		 assert(hasNumberOfCapturedStones(C,white,0)).

allStones(Position,L) :- findall(Stone,isInSquare(Position,Stone,_,_),L).

copyStones(_P1,_P2,[]).
copyStones(P1,P2,[Stone|L]) :- isInSquare(P1,Stone,I,J),
			     assert(isInSquare(P2,Stone,I,J)),
			     copyStones(P1,P2,L).

% copies an entire board
copyBoard(Pin,Pout) :- cleanBoard(Pout),
	               allStones(Pin,L),copyStones(Pin,Pout,L),
		       retractall(hasNumberMoves(Pout,_,_)),
	               hasNumberMoves(Pin,black,B),assert(hasNumberMoves(Pout,black,B)),
		       hasNumberMoves(Pin,white,W),assert(hasNumberMoves(Pout,white,W)),
		       hasPlayerWithTheMove(Pin,C),assert(hasPlayerWithTheMove(Pout,C)),
		       hasNumberOfCapturedStones(Pin,black,CB),
		       retractall(hasNumberOfCapturedStones(Pout,_,_)),
		       assert(hasNumberOfCapturedStones(Pout,black,CB)),
		       hasNumberOfCapturedStones(Pin,white,CW),
		       assert(hasNumberOfCapturedStones(Pout,white,CW)).
