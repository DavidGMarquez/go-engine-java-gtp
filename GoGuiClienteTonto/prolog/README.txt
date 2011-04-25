Distintas partidas guardadas. Ver el tablero con printBoard(0), antes y después de las jugadas indicadas.

goGame.pl
---------
Partida con varias opciones de jugadas. Jugar con la ficha negra, con play(fila,columna) o con placeStone(0,black,fila,columna):
play(1,a).
placeStone(0,black,1,a).

Si se usa play, observar que white ha capturado el bloque más grande de piezas negras.
Si no, usar chooseMove(0,levelone,white,X,Y) para ver que la jugada generada siempre será la que más piezas negras captura.

goGame_blackpass.pl
-------------------
Partida con un tablero casi lleno de piezas blancas, excepto dos huecos. Observar con chooseMove(0,levelone,black,X,Y) que la única opción que tiene es pasar (0,0), ya que cualquier otra opción sería suicidio.

goGame_capture.pl
-----------------
Distintas formas de suicidio.
1. Con placeStoneWithoutMove(0,white,2,b), comprobar que la pieza blanca no se queda en el tablero, y es capturada por la negra - comprobar con hasNumberOfCapturedStones(0,black,N)
2. Con placeStoneWithoutMove(0,white,4,f), comprobar que ambas piezas blancas son capturadas, y el número de piezas capturadas por las negras se ha incrementado en 2.
3. Con chooseMove(0,levelone,white,X,Y), comprobar que devuelve (11,10), suicidio que le lleva a capturar 4 fichas negras. Con placeStoneWithoutMove(0,white,11,k), comprobar que desaparecen las fichas negras, pero no la blanca nueva, y que se suman a la puntuación.

goGame_capturetwo.pl
--------------------
Capturar dos conjuntos de piezas opuestas con una sola ficha.
Con placeStoneWithoutMove(0,black,4,d), comprobar que se capturan todas las fichas blancas.

goGame_gensmartmoves.pl
-----------------------
El generador de jugadas de nivel dos evita jugadas potencialmente peligrosas.
En el tablero hay tres posiciones en las que se pueden colocar las blancas que harían que en la siguiente jugada fueran capturadas por las negras:
3,1  (3,a)
2,2  (2,b)
1,3  (1,c)
Además está la jugada suicida, 1,1 (1,a), que se evitaría también con el nivel uno de evaluación.
Comprobar que chooseMoves(0,levelone,white,L) devuelve todos los movimientos (excepto el suicidio) mientras que el nivel 2, chooseMoves(0,leveltwo,white,L), no devuelve las jugadas potencialmente peligrosas.