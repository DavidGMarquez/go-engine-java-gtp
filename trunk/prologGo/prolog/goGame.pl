:-consult('goRules.pl').
:- assert(initialConfiguration(0)).
:- assert(isInSquare(0, white(1), 2, 5)).
% :- assert(isInSquare(0, white(2), 1, 4)).
:- assert(isInSquare(0, white(3), 1, 6)).
:- assert(isInSquare(0, white(4), 1, 7)).
:- assert(isInSquare(0, white(5), 1, 8)).
:- assert(isInSquare(0, white(6), 5, 6)).
% :- assert(isInSquare(0, white(7), 4, 8)).
:- assert(isInSquare(0, white(8), 5, 7)).
:- assert(isInSquare(0, white(9), 4, 4)).
:- assert(isInSquare(0, white(10), 3, 6)).
:- assert(isInSquare(0, white(11), 3, 7)).
:- assert(isInSquare(0, white(12), 3, 5)).
:- assert(isInSquare(0, white(13), 5, 5)).
:- assert(isInSquare(0, white(14), 9, 9)).
:- assert(isInSquare(0, white(15), 8, 8)).
:- assert(isInSquare(0, white(16), 8, 9)).
:- assert(isInSquare(0, white(17), 9, 8)).

:- assert(isInSquare(0, black(1), 1, 5)).
:- assert(isInSquare(0, black(2), 4,6)).
:- assert(isInSquare(0, black(3), 4,7)).
:- assert(isInSquare(0, black(4), 4,5)).

:- assert(hasPlayerWithTheMove(0,black)).
:- assert(hasNumberMoves(0,black,6)).
:- assert(hasNumberMoves(0,white,17)).
:- assert(hasNumberOfCapturedStones(0,white,0)).
:- assert(hasNumberOfCapturedStones(0,black,0)).

% Print the entire board
printBoard(P) :- initialConfiguration(C), hasSquareSize(C,S),
	         write('    '), printHeaderRow(1),
		 write('   +'), printGridEdge(S),
		                printRank(P,S),
		 write('   +'), printGridEdge(S).

% Prints the last element in the header row
printHeaderRow(S) :- initialConfiguration(C), hasSquareSize(C,S),printLetter(S),
	             writeln(' '),!.
% Prints the header row (A-T)
printHeaderRow(N) :- initialConfiguration(C), hasSquareSize(C,S), N=<S,
	             printLetter(N), write(' '),
		     N1 is N+1, printHeaderRow(N1).

printLetter(N) :- isFile(N,L),write(L).
printNumber(N) :- N =< 9, write(' '),write(N).
printNumber(N) :- N >= 10, write(N).

% Prints the horizontal edge of the grid: +-----------+
printGridEdge(1) :- write('-+'),writeln(' '),!.
printGridEdge(S) :- write('--'),
	            S1 is S-1, printGridEdge(S1).

% Prints the lowest rank
printRank(P,1) :- write(' 1 |'),printFile(P,1,1),writeln(' ').
% Prints the following ranks until lineInGrid is false
printRank(P,I) :- printNumber(I),write(' |'),
	          lineInGrid(I),printFile(P,I,1),
		  writeln(' '),
		  I1 is I-1, printRank(P,I1).

% Prints the last square in a rank
printFile(P,I,M) :- isUEdge(M),
		    printStone(P,I,M),write('|').

% Prints squares in rank I recursively, with file J
printFile(P,I,J) :- lineInGrid(I),lineInGrid(J),
	            printStone(P,I,J),write('.'),
		    J1 is J+1, printFile(P,I,J1).

printStone(P,I,J) :- (not(isInSquare(P,_S,I,J)),write(' '));
		     (isInSquare(P,white(_X),I,J),write('W'));
		     (isInSquare(P,black(_Y),I,J),write('B')).











