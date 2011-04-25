Distintas partidas guardadas. Ver el tablero con printBoard(0), antes y despu�s de las jugadas indicadas.

goGame.pl
---------
Partida con varias opciones de jugadas. Jugar con la ficha negra, con play(fila,columna) o con placeStone(0,black,fila,columna):
play(1,a).
placeStone(0,black,1,a).

Si se usa play, observar que white ha capturado el bloque m�s grande de piezas negras.
Si no, usar chooseMove(0,levelone,white,X,Y) para ver que la jugada generada siempre ser� la que m�s piezas negras captura.

goGame_capture.pl
-----------------
Distintas formas de suicidio.
1. Con placeStoneWithoutMove(0,white,2,b), comprobar que la pieza blanca no se queda en el tablero, y es capturada por la negra - comprobar con hasNumberOfCapturedStones(0,black,N)
2. Con placeStoneWithoutMove(0,white,4,f), comprobar que ambas piezas blancas son capturadas, y el n�mero de piezas capturadas por las negras se ha incrementado en 2.
3. Con chooseMove(0,levelone,white,X,Y), comprobar que devuelve (11,10), suicidio que le lleva a capturar 4 fichas negras. Con placeStoneWithoutMove(0,white,11,k), comprobar que desaparecen las fichas negras, pero no la blanca nueva, y que se suman a la puntuaci�n.

goGame_capturetwo.pl
--------------------
Capturar dos conjuntos de piezas opuestas con una sola ficha.
Con placeStoneWithoutMove(0,black,4,d), comprobar que se capturan todas las fichas blancas.

goGame_gensmartmoves.pl
-----------------------
El generador de jugadas de nivel dos evita jugadas potencialmente peligrosas.
En el tablero hay tres posiciones en las que se pueden colocar las blancas que har�an que en la siguiente jugada fueran capturadas por las negras:
3,1  (3,a)
2,2  (2,b)
1,3  (1,c)
Adem�s est� la jugada suicida, 1,1 (1,a), que se evitar�a tambi�n con el nivel uno de evaluaci�n.
Comprobar que chooseMoves(0,levelone,white,L) devuelve todos los movimientos (excepto el suicidio) mientras que el nivel 2, chooseMoves(0,leveltwo,white,L), no devuelve las jugadas potencialmente peligrosas.

goGame_ko.pl
------------
En el tablero est� la cl�sica posici�n de Ko. Introducir:
placeStoneWithoutMove(0,white,3,b).
placeStoneWithoutMove(0,black,3,c).
Ya no se puede colocar una pieza blanca en (3,b), al menos de que se ponga antes una pieza en otro sitio.
Comprobar con chooseMoves que la posici�n de Ko no est� incluida, y por lo tanto no podr� ser generada:
chooseMoves(0,leveltwo,white,L).
Comprobar que la mejor opci�n para el color negro es colocar una pieza en el lugar (3,c), ya que si la colocan en cualquier otro sitio, las blancas ya son libres de volver a colocar su pieza ah� para capturar la negra:
chooseMoves(0,leveltwo,black,L).

goGame_badmove.pl
-----------------
Las �nicas opciones que tienen las fichas blancas har�n que las negras las capturen. La decisi�n que tome depende del nivel de evaluaci�n.
Si utiliza un nivel uno, lo pondr� en cualquier espacio vac�o:
chooseMoves(0,levelone,white,L).
Si utiliza el nivel 2, no tomar� la opci�n de colocarla en (5,2), ya que har�a que las negras le capturaran 5 piezas en vez de tres. Tambi�n contempla la opci�n de pasar, que har� que pierda 3 piezas igualmente.

goGame_gensmartneutralmoves.pl
------------------------------
Selecci�n de movimientos neutros en funci�n de cu�ntas piezas adyacentes propias u opuestas hay.
Probar con:
chooseMoves(0,leveltwo,white,L).
y
chooseMoves(0,leveltwo,black,L).
En algunas ocasiones se agrupar� con las fichas opuestas, y en otras con las propias.