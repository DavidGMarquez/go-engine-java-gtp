Distintas partidas guardadas. Ver el tablero con printBoard(0), antes y despu�s de las jugadas indicadas.

goGame.pl
---------
Partida con varias opciones de jugadas. Jugar con la ficha negra, con play(fila,columna) o con placeStone(0,black,fila,columna):
play(1,a).
placeStone(0,black,1,a).

Si se usa play, observar que white ha capturado el bloque m�s grande de piezas negras.
Si no, usar chooseMove(0,white,X,Y) para ver que la jugada generada siempre ser� la que m�s piezas negras captura.

goGame_blackpass.pl
-------------------
Partida con un tablero casi lleno de piezas blancas, excepto dos huecos. Observar con chooseMove(0,black,X,Y) que la �nica opci�n que tiene es pasar (0,0), ya que cualquier otra opci�n ser�a suicidio.

goGame_capture.pl
-----------------
Distintas formas de suicidio.
1. Con placeStoneWithoutMove(0,white,2,b), comprobar que la pieza blanca no se queda en el tablero, y es capturada por la negra - comprobar con hasNumberOfCapturedStones(0,black,N)
2. Con placeStoneWithoutMove(0,white,4,f), comprobar que ambas piezas blancas son capturadas, y el n�mero de piezas capturadas por las negras se ha incrementado en 2.
3. Con chooseMove(0,white,X,Y), comprobar que devuelve (11,10), suicidio que le lleva a capturar 4 fichas negras. Con placeStoneWithoutMove(0,white,11,k), comprobar que desaparecen las fichas negras, pero no la blanca nueva, y que se suman a la puntuaci�n.

goGame_capturetwo.pl
--------------------
Capturar dos conjuntos de piezas opuestas con una sola ficha.
Con placeStoneWithoutMove(0,black,4,d), comprobar que se capturan todas las fichas blancas.