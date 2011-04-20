package prologlink;

import jpl.*;

/**
 *
 * @author Cayette
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        String path = "prolog/goRules.pl";
        Queries q = new Queries();
        String t1 = "consult('" + path + "')";
        Query q1 = new Query(t1);
        System.out.println(t1 + " " + (q1.hasSolution() ? "succeeded" : "failed"));

        q.newGame();
        if(q.boardsize(9)){
            System.out.println("Boardsize changed");
        }else{
            System.out.println("Illegal boardsize");
        }

        String t3 = "hasPlayerWithTheMove(0,black)";
        Query q3 = new Query(t3);
        System.out.println(t3 + " " + (q3.hasSolution() ? "succeeded" : "failed"));
        q3.close();
        Move m = new Move(1, 1, "black");

        if (q.addPiece(m) == false) {
            System.out.println("Movimiento ilegal");
        } else {
            System.out.println(m.toPlayString());
        }
        Move m2 = q.genMove("black");
        System.out.println("genmove B\n= "+m2.toGenString());
        m2 = q.genMove("white");
        System.out.println("genmove W\n= "+m2.toGenString());
        m2 = q.genMove("black");
        System.out.println("genmove B\n= "+m2.toGenString());
        m2 = q.genMove("white");
        System.out.println("genmove W\n= "+m2.toGenString());
        m2 = q.genMove("black");
        System.out.println("genmove B\n= "+m2.toGenString());
        m2 = q.genMove("white");
        System.out.println("genmove W\n= "+m2.toGenString());
        m2 = q.genMove("black");
        System.out.println("genmove B\n= "+m2.toGenString());
    }
}
