Distintas partidas guardadas. Ver el tablero con printBoard(0), antes y después de las jugadas indicadas.

goGame.pl
---------
Partida con varias opciones de jugadas. Jugar con la ficha negra, con play(fila,columna) o con placeStone(0,black,fila,columna):
play(1,a).
placeStone(0,black,1,a).

Si se usa play, observar que white ha capturado el bloque más grande de piezas negras.
Si no, usar chooseMove(0,levelone,white,X,Y) para ver que la jugada generada siempre será la que más piezas negras captura.

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

goGame_ko.pl
------------
En el tablero está la clásica posición de Ko. Introducir:
placeStoneWithoutMove(0,white,3,b).
placeStoneWithoutMove(0,black,3,c).
Ya no se puede colocar una pieza blanca en (3,b), al menos de que se ponga antes una pieza en otro sitio.
Comprobar con chooseMoves que la posición de Ko no está incluida, y por lo tanto no podrá ser generada:
chooseMoves(0,leveltwo,white,L).
Comprobar que la mejor opción para el color negro es colocar una pieza en el lugar (3,c), ya que si la colocan en cualquier otro sitio, las blancas ya son libres de volver a colocar su pieza ahí para capturar la negra:
chooseMoves(0,leveltwo,black,L).

goGame_badmove.pl
-----------------
Las únicas opciones que tienen las fichas blancas harán que las negras las capturen. La decisión que tome depende del nivel de evaluación.
Si utiliza un nivel uno, lo pondrá en cualquier espacio vacío:
chooseMoves(0,levelone,white,L).
Si utiliza el nivel 2, no tomará la opción de colocarla en (5,2), ya que haría que las negras le capturaran 5 piezas en vez de tres. También contempla la opción de pasar, que hará que pierda 3 piezas igualmente.

goGame_gensmartneutralmoves.pl
------------------------------
Selección de movimientos neutros en función de cuántas piezas adyacentes propias u opuestas hay.
Probar con:
chooseMoves(0,leveltwo,white,L).
y
chooseMoves(0,leveltwo,black,L).
En algunas ocasiones se agrupará con las fichas opuestas, y en otras con las propias.