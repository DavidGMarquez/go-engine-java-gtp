:- consult('listOperations.pl').

% Checks the turn on a board for a color
% (Board, Color)
:- dynamic hasPlayerWithTheMove/2.

% Checks the number of moves on a board for a color
% (Board,Color,NumberOfMoves)
:- dynamic hasNumberMoves/3.

% Checks whether a stone is placed on the board on a rank and file
% (Board, Stone, Rank, File)
:- dynamic isInSquare/4.

% Checks the number of stones a player has captured
% (Board, Color, NumberofCapturedStones)
:- dynamic hasNumberOfCapturedStones/3.

% Checks how many consecutive passes there have been
% (Board, Passes)
:- dynamic passes/2.

% Establece la configuracion por defecto del tablero 
:- dynamic initialConfiguration/1.

% Defines a stone as a color with an associated number
% based on order of placement on the board
stone(white(X)) :- integer(X), X>=0.
stone(black(X)) :- integer(X), X>=0.

% Definitions of all the board lines, horizontal and vertical
% Horizontal ranks are numbered from top to botton
% Vertical files are numbered from left to right
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

% Number/letter conversion, for files
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


% Available boardsizes.
% The first arg is used with initialConfiguration/1
% The second is the boardsize
hasSquareSize(0,9).
hasSquareSize(1,13).
hasSquareSize(2,19).
hasSquareSize(3,4).
hasSquareSize(4,5).
hasSquareSize(5,6).
hasSquareSize(6,7).
hasSquareSize(7,8).
hasSquareSize(8,10).
hasSquareSize(9,11).
hasSquareSize(10,12).
hasSquareSize(11,14).
hasSquareSize(12,15).
hasSquareSize(13,16).
hasSquareSize(14,17).
hasSquareSize(15,18).

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
isAdyacent(Position,I,J,Stone) :- isInSquare(Position,Stone,I2,J2),
				       ((I=:=I2,abs(J-J2)=:=1);J=:=J2,abs(I-I2)=:=1).

% Checks whether two adyacent stones have the same color
isAdyacentSame(Position,Stone1,Stone2) :- isAdyacent(Position,Stone1,Stone2),
	                                  hasColor(Stone1,Color), hasColor(Stone2,Color).

% Checks whether two adyacent stones DON'T have the same color
isAdyacentOpposite(Position,Stone1,Stone2) :- isAdyacent(Position,Stone1,Stone2),
	                                      not(isAdyacentSame(Position,Stone1,Stone2)).
isAdyacentOpposite(Position,Color,I,J,Stone) :- isAdyacent(Position,I,J,Stone),
	                                        isOppositeColor(Color,OtherColor),
						hasColor(Stone,OtherColor).

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

% Checks if a stone or coordinates have NO adyacent stones
isFree(Position,Stone) :- not(isAdyacent(Position,Stone,_)).
isFree(Position,I,J) :- not(isAdyacent(Position,I,J,_)).

% Legal moves.
% 0,0 is equivalent to passing
passingMove((0,0)).
legalMove(_,_,0,0).
legalMove(Position, _Color, I, J) :- lineInGrid(I), lineInGrid(J),
	                            not(isInSquare(Position,_S,I,J)).

% Returns a list of legal moves, checking for Ko situation
% legalMoves(Position,Color,L) :-
% findall((I,J),(legalMove(Position,Color,I,J),not(isKo(Position,ko,Color,I,J))),L).
% 

% Returns a list of legal moves, not checking for Ko situation
legalMoves(Position,Color,L) :- findall((I,J),(legalMove(Position,Color,I,J)),L).
						

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

% Lists the liberties of stones in L on Lout, without copies
listLiberties(P,[Stone|L],Lin,Lout) :- listAdyacentEmpty(P,Stone,Le),
	                               addPairs(Le,Lin,LoutA),
				       listLiberties(P,L,LoutA,Lout),!.
listLiberties(_P,_,Lin,Lin).


% ACTIONS

% Passing stone
placeStone(P,Color,0,0) :- hasPlayerWithTheMove(P,Color),
	                   retractall(hasPlayerWithTheMove(P,Color)),
			   isOppositeColor(Color,OtherColor),
			   assert(hasPlayerWithTheMove(P,OtherColor)),
			   passes(P,Pas),
			   Passes is Pas+1,
			   retractall(passes(P,_)),
			   assert(passes(P,Passes)),!.

% Places a stone on (I,J), checking whether it is Color's turn
placeStone(P,Color,I,J) :- legalMove(P,Color,I,J),
	                   not(isKo(P,ko,Color,I,J)),
	                   hasNumberMoves(P,Color,X),
			   N is X+1,
			   retractall(hasNumberMoves(P,Color,X)),
			   assert(hasNumberMoves(P,Color,N)),
			   hasPlayerWithTheMove(P,Color),
			   hasColor(Stone,Color),
			   hasNumber(Stone,N),
			   assert(isInSquare(P,Stone,I,J)),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkOwnCapture(P,Stone),
  			   retractall(hasPlayerWithTheMove(P,Color)),
			   isOppositeColor(Color,OtherColor),
			   assert(hasPlayerWithTheMove(P,OtherColor)),
			   copyBoard(anterior,ko),
			   copyBoard(P,anterior),
			   not(passingMove((I,J))),
			   retractall(passes(P,_)),
			   assert(passes(P,0)),!.

% L is the letter representing the File (a-t)
placeStone(P,Color,I,L) :- isFile(J,L),placeStone(P,Color,I,J),!.

% Passing stone
placeStoneWithoutMove(P,Color,0,0) :-
	                   retractall(hasPlayerWithTheMove(P,Color)),
			   isOppositeColor(Color,OtherColor),
			   assert(hasPlayerWithTheMove(P,OtherColor)),
			   passes(P,Pas),
			   Passes is Pas+1,
			   retractall(passes(P,_)),
			   assert(passes(P,Passes)),!.


% Places a stone on (I,J) regardless of whether it's Color's turn or not
placeStoneWithoutMove(P,Color,I,J) :-
	                   legalMove(P,Color,I,J),
			   not(isKo(P,ko,Color,I,J)),
	                   hasNumberMoves(P,Color,X),
			   N is X+1,
			   retractall(hasNumberMoves(P,Color,X)),
			   assert(hasNumberMoves(P,Color,N)),
			   hasColor(Stone,Color),
			   hasNumber(Stone,N),
			   assert(isInSquare(P,Stone,I,J)),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkOwnCapture(P,Stone),
			   retractall(hasPlayerWithTheMove(P,Color)),
			   isOppositeColor(Color,OtherColor),
			   assert(hasPlayerWithTheMove(P,OtherColor)),
			   copyBoard(anterior,ko),
			   copyBoard(P,anterior),
			   not(passingMove((I,J))),
			   retractall(passes(P,_)),
			   assert(passes(P,0)),!.

% L is the letter representing the File (a-t)
placeStoneWithoutMove(P,Color,I,L) :- isFile(J,L),placeStoneWithoutMove(P,Color,I,J),!.

% Passing stone
placeStoneWithoutCopy(P,Color,0,0) :-
	                   retractall(hasPlayerWithTheMove(P,Color)),
			   isOppositeColor(Color,OtherColor),
			   assert(hasPlayerWithTheMove(P,OtherColor)),
			   passes(P,Pas),
			   Passes is Pas+1,
			   retractall(passes(P,_)),
			   assert(passes(P,Passes)),!.

% Places a stone on (I,J) without checking for ko
% Is used in virtual boards, levelone, leveltwo and koeval
placeStoneWithoutCopy(P,Color,I,J) :-
	                   legalMove(P,Color,I,J),
	                   hasNumberMoves(P,Color,X),
			   N is X+1,
			   retractall(hasNumberMoves(P,Color,X)),
			   assert(hasNumberMoves(P,Color,N)),
			   hasColor(Stone,Color),
			   hasNumber(Stone,N),
			   assert(isInSquare(P,Stone,I,J)),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkCaptured(P,Stone),
			   checkOwnCapture(P,Stone),
			   retractall(hasPlayerWithTheMove(P,Color)),
			   isOppositeColor(Color,OtherColor),
			   assert(hasPlayerWithTheMove(P,OtherColor)),
			   not(passingMove((I,J))),
			   retractall(passes(P,_)),
			   assert(passes(P,0)),!.

% L is the letter representing the File (a-t)
placeStoneWithoutCopy(P,Color,I,L) :- isFile(J,L),placeStoneWithoutCopy(P,Color,I,J),!.


% Checks if the stone has captured any stones around it
checkCaptured(P,Stone) :- isAdyacentOpposite(P,Stone,Stone2),
	                  listSurrounded(P,Stone2,L),
			  removeStones(P,L).

% Returns true so as to not interrupt the stone placing process
checkCaptured(_,_).

% Returns in L a list of stones that should get captured
% Does NOT capture said stones
checkIfCapture(P,Stone,L) :- listSurrounded(P,Stone,L).
checkIfCapture(_,_,[]).

% Removes captured stones that have the same color as Stone
checkOwnCapture(P,Stone) :- listSurrounded(P,Stone,L),
			    removeStones(P,L).
checkOwnCapture(_,_).

% Removes a list of stones, adding the points to the opposite color
removeStones(_,[]).
removeStones(Position,[Stone|List]) :- 
	isInSquare(Position,Stone,_,_),
	retractall(isInSquare(Position,Stone,_,_)),
	addPoints(Position,Stone),
	removeStones(Position,List).

% Adds points for each stone captured
addPoints(Position,Stone) :- hasColor(Stone,Color),
	hasNumberOfCapturedStones(Position,Color2,X),
	isOppositeColor(Color,Color2),
	N is X+1,
	retractall(hasNumberOfCapturedStones(Position,Color2,X)),
	assert(hasNumberOfCapturedStones(Position,Color2,N)).

% Checks whether the boards P and Ko have same colored stones
% in the same position
isKo(P,Ko,Color,I,J) :-
	copyBoard(P,koeval),
	placeStoneWithoutCopy(koeval,Color,I,J),
	areEqualBoards(koeval,Ko),
	cleanBoard(koeval),!.

% Resets the game
cleanBoard(C) :- retractall(isInSquare(C,_,_,_)),
	         retractall(hasNumberMoves(C,_,_)),
		 retractall(hasNumberOfCapturedStones(C,_,_)),
		 retractall(passes(C,_)),
		 assert(passes(C,0)),
		 assert(hasNumberMoves(C,black,0)),
		 assert(hasNumberMoves(C,white,0)),
		 assert(hasNumberOfCapturedStones(C,black,0)),
		 assert(hasNumberOfCapturedStones(C,white,0)),
		 assert(hasPlayerWithTheMove(C,black)).

% Changes the size of the board
changeSize(Size) :- retractall(initialConfiguration(_)),assert(initialConfiguration(Size)).

% Returns all of the stones present in Position, in list L
allStones(Position,L) :- findall(Stone,isInSquare(Position,Stone,_,_),L).

% Returns all of the stones present in Position with color Color
allStonesSquare(Position,Color,L) :- findall((I,J),(isInSquare(Position,Stone,I,J),hasColor(Stone,Color)),L).

% Copies the stones from a list into P2, provided they exist in P1
copyStones(_P1,_P2,[]).
copyStones(P1,P2,[Stone|L]) :- isInSquare(P1,Stone,I,J),
			     assert(isInSquare(P2,Stone,I,J)),
			     copyStones(P1,P2,L).

% Checks whether a board has been initialized
checkInit(P) :- hasNumberMoves(P,_,_),
	        hasPlayerWithTheMove(P,_),
		hasNumberOfCapturedStones(P,_,_),!.
checkInit(P) :- cleanBoard(P),!.

% copies an entire board
copyBoard(Pin,Pout) :- cleanBoard(Pout),
	               checkInit(Pin),
	               allStones(Pin,L),copyStones(Pin,Pout,L),
		       retractall(hasNumberMoves(Pout,_,_)),
	               hasNumberMoves(Pin,black,B),assert(hasNumberMoves(Pout,black,B)),
		       hasNumberMoves(Pin,white,W),assert(hasNumberMoves(Pout,white,W)),
		       retractall(hasPlayerWithTheMove(Pout,_)),
		       hasPlayerWithTheMove(Pin,C),
		       assert(hasPlayerWithTheMove(Pout,C)),
		       hasNumberOfCapturedStones(Pin,black,CB),
		       retractall(hasNumberOfCapturedStones(Pout,_,_)),
		       assert(hasNumberOfCapturedStones(Pout,black,CB)),
		       hasNumberOfCapturedStones(Pin,white,CW),
		       assert(hasNumberOfCapturedStones(Pout,white,CW)).

% Checks whether two boards P1 and P2 have same colored stones
% placed on the same positions
areEqualBoards(P1,P2) :- allStonesSquare(P1,white,L1W),allStonesSquare(P2,white,L2W),
	                 allStonesSquare(P1,black,L1B),allStonesSquare(P2,black,L2B),
			 equal(L1W,L2W),equal(L1B,L2B),!.

% Checks whether there have been at least two consecutive passes
finishedGame(P) :- passes(P,N),N>1.





% INTELLIGENCE

% Evaluates a possible move in board P
% Generates the best possible move for the opponent
% Uses a Lv1 evaluator for the opponent's move
evaluateVirtualTablet(P,PV,leveltwo,Color,I,J,Points,OPoints,NPoints,NPoints2) :-
	isOppositeColor(Color,OtherColor),
	copyBoard(P,PV),
	placeStoneWithoutCopy(PV,Color,I,J),
	chooseMove(PV,levelone,OtherColor,X,Y),
	placeStoneWithoutCopy(PV,OtherColor,X,Y),
	((isAdyacentOpposite(PV,Color,I,J,Stone),isInSquare(PV,Stone,X,Y),
	listConnected(PV,OtherColor,X,Y,[],L),numberOfElements(L,NPoints2));
	(NPoints2 is 0)),
	listConnected(PV,Color,I,J,[],L),
	numberOfElements(L,NPoints),
	hasNumberOfCapturedStones(PV,Color,Points),
	hasNumberOfCapturedStones(PV,OtherColor,OPoints),
	cleanBoard(PV),!.

% Evaluates a possible move in board P
evaluateVirtualTablet(P,PV,levelone,Color,I,J,Points,OPoints,NPoints,NPoints2):-
	copyBoard(P,PV),
	placeStoneWithoutCopy(PV,Color,I,J),
	((isAdyacentOpposite(PV,Color,I,J,Stone),isInSquare(PV,Stone,X,Y),
	listConnected(PV,OtherColor,X,Y,[],L),numberOfElements(L,NPoints2));
	(NPoints2 is 0)),
	listConnected(PV,Color,I,J,[],L),
	numberOfElements(L,NPoints),
	hasNumberOfCapturedStones(PV,Color,Points),
	isOppositeColor(Color,OtherColor),
	hasNumberOfCapturedStones(PV,OtherColor,OPoints),
	cleanBoard(PV),!.


% Evaluates a move by assigning it a score
% The score is the number of opposite stones that will be captured.
% The neutral score is selected randomly between adyacent stones and
% opposite adyacent stones
evaluateMove(P,Level,Color,(I,J),N,Neutral) :- hasNumberOfCapturedStones(P,Color,C1Own),
	                         isOppositeColor(Color,OtherColor),
				 hasNumberOfCapturedStones(P,OtherColor,C1Other),
	                         evaluateVirtualTablet(P,Level,Level,Color,I,J,C2Own,C2Other,N1,N2),
				 randomElement([N1,N2],Neutral),
				 OwnGain is C2Own-C1Own,
				 OtherGain is C2Other-C1Other,
			         N is OwnGain-OtherGain.

% Evaluates a move, deciding if it's positive, neutral or negative

% positive move: has positive capture score 
positiveMove(P,Level,Color,(I,J),N):- evaluateMove(P,Level,Color,(I,J),N,_),N>0.

% neutral move: place a stone with 4 liberties, affecting no other stone
neutralMove(P,levelone,_Color,(I,J),0) :- isFree(P,I,J),
	                                  not(passingMove((I,J))),!.
% Neutral move: no stones are captured
neutralMove(P,Level,Color,(I,J),N) :- evaluateMove(P,Level,Color,(I,J),0,N),not(passingMove((I,J))).

% Negative move: the player loses stones
negativeMove(P,Level,Color,(I,J),N):- evaluateMove(P,Level,Color,(I,J),N,_),N<0.
		

% Returns in a list all of the moves that will capture opposing stones
positiveMoves(_,_,_,[],L,L,P,P).
positiveMoves(P,Level,Color,[Move|Allmoves],PositiveMoves,PositiveMovesOut,PointsIn,PointsOut) :-
	((positiveMove(P,Level,Color,Move,N),append(PositiveMoves,[Move],PositiveMovesO),append(PointsIn,[N],PO));
	(not(positiveMove(P,Level,Color,Move,_)),append(PositiveMoves,[],PositiveMovesO),append(PointsIn,[],PO))),
	positiveMoves(P,Level,Color,Allmoves,PositiveMovesO,PositiveMovesOut,PO,PointsOut),!.

% Returns in a list all of the moves that will capture own stones
negativeMoves(_,_,_,[],L,L,P,P).
negativeMoves(P,Level,Color,[Move|Allmoves],NegativeMoves,NegativeMovesOut,PointsIn,PointsOut) :-
	((negativeMove(P,Level,Color,Move,N),append(NegativeMoves,[Move],NegativeMovesO),append(PointsIn,[N],PO));
	(not(negativeMove(P,Level,Color,Move,_)),append(NegativeMoves,[],NegativeMovesO),append(PointsIn,[],PO))),
	negativeMoves(P,Level,Color,Allmoves,NegativeMovesO,NegativeMovesOut,PO,PointsOut),!.


% Returns in a list all of the moves that will capture no stones
neutralMoves(_,_,_,[],L,L,P,P).
neutralMoves(P,Level,Color,[Move|Allmoves],NeutralMoves,NeutralMovesOut, PointsIn,PointsOut) :-
	((neutralMove(P,Level,Color,Move,N),append(NeutralMoves,[Move],NeutralMovesO),append(PointsIn,[N],PO));
	(not(neutralMove(P,Level,Color,Move,_)),append(NeutralMoves,[],NeutralMovesO),append(PointsIn,[],PO))),
	neutralMoves(P,Level,Color,Allmoves,NeutralMovesO,NeutralMovesOut,PO,PointsOut),!.


% Picks any neutral move from the list of legal moves
anyMove(P,Level,Color,Move) :-
	legalMoves(P,Color,L),
	neutralMoves(P,Level,Color,L,[],NM,[],_),
	randomElement(NM,Move).

% Returns a list of all neutral moves
anyMoves(P,Level,Color,Moves) :-
        legalMoves(P,Color,L),
	neutralMoves(P,Level,Color,L,[],Moves,[],_),
	numberOfElements(Moves,N),N>0.

% Returns the neutral move with the highest neutral score
bestNeutralMove(P,Level,Color,Move) :-
	legalMoves(P,Color,L),
	neutralMoves(P,Level,Color,L,[],NM,[],Points),
	maxList(Points,Max),
	find_indexes(Points,Max,In),
	getElementAt(In,NM,Moves),
	randomElement(Moves,Move).

% Returns a list of the neutral moves with the highest score
bestNeutralMoves(P,Level,Color,Moves):-
	legalMoves(P,Color,L),
	neutralMoves(P,Level,Color,L,[],NM,[],Points),
	maxList(Points,Max),
	find_indexes(Points,Max,In),
	getElementAt(In,NM,Moves).

% Picks any positive move from the list of legal moves
anyPositiveMove(P,Level,Color,Move) :-
	legalMoves(P,Color,L),
	positiveMoves(P,Level,Color,L,[],PM,[],_),
	randomElement(PM,Move).

% Best positive move
bestMove(P,Level,Color,Move) :-
	legalMoves(P,Color,L),
	positiveMoves(P,Level,Color,L,[],PM,[],Points),
	maxList(Points,Max),
	find_indexes(Points,Max,In),
	getElementAt(In,PM,Moves),
	randomElement(Moves,Move).

% Best positive moves
bestMoves(P,Level,Color,Moves):-
	legalMoves(P,Color,L),
	positiveMoves(P,Level,Color,L,[],PM,[],Points),
	maxList(Points,Max),
	find_indexes(Points,Max,In),
	getElementAt(In,PM,Moves).

% Picks any negative move from the list of legal moves
anyNegativeMove(P,Level,Color,Move) :-
	legalMoves(P,Color,L),
	negativeMoves(P,Level,Color,L,[],PM,[],_),
	randomElement(PM,Move).

% Best negative move
bestBadMove(P,Level,Color,Move) :-
	legalMoves(P,Color,L),
	negativeMoves(P,Level,Color,L,[],NM,[],Points),
	maxList(Points,Max),
	find_indexes(Points,Max,In),
	getElementAt(In,NM,Moves),
	randomElement(Moves,Move).

% Best negative moves
bestBadMoves(P,Level,Color,Moves):-
	legalMoves(P,Color,L),
	negativeMoves(P,Level,Color,L,[],NM,[],Points),
	maxList(Points,Max),
	find_indexes(Points,Max,In),
	getElementAt(In,NM,Moves).

% Chooses a random move, based on score
chooseMove(P,Level,Color,X,Y) :- bestMove(P,Level,Color,(X,Y)),!.
chooseMove(P,Level,Color,X,Y) :- bestNeutralMove(P,Level,Color,(X,Y)),!.
chooseMove(P,Level,Color,X,Y) :- bestBadMove(P,Level,Color,(X,Y)),!.
chooseMove(_,_,_,0,0).

% returns a list of moves
chooseMoves(P,Level,Color,L):- bestMoves(P,Level,Color,L),!.
chooseMoves(P,Level,Color,L):- bestNeutralMoves(P,Level,Color,L),!.
chooseMoves(P,Level,Color,L):- bestBadMoves(P,Level,Color,L),!.
chooseMoves(_,_,_,[(0,0)]).
	   
% Black plays on board 0
% White chooses the best move possible and plays
play(I,J) :- placeStone(0,black,I,J),
	     chooseMove(0,levelone,white,X,Y),
	     placeStone(0,white,X,Y).		       
