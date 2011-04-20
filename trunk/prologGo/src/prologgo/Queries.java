package prologgo;

import java.util.Hashtable;
import jpl.Query;

/**
 *
 * @author Cayette
 */
public class Queries {

    public Queries() {
    }

    public void newGame() {
        Query q = new Query("cleanBoard(0)");
        q.hasSolution();
        q = new Query("assert(hasPlayerWithTheMove(0,black))");
        q.hasSolution();
        q = new Query("assert(hasNumberMoves(0,white,0))");
        q.hasSolution();
        q = new Query("assert(hasNumberMoves(0,black,0))");
        q.hasSolution();
        q = new Query("assert(hasNumberOfCapturedStones(0,white,0))");
        q.hasSolution();
        q = new Query("assert(hasNumberOfCapturedStones(0,black,0))");
        q.hasSolution();
    }

    public boolean addPiece(String color, int rank, char file) {
        String t = "placeStone(0," + color + "," + file + "," + rank + ")";
        Query q = new Query(t);
        if (q.hasSolution()) {
            return true;
        } else {
            return false;
        }
    }

    public boolean addPiece(Move m) {
        String t = "placeStone(0," + m.getColor() + "," + m.getFile() + "," + m.getRank() + ")";
        Query q = new Query(t);
        if (q.hasSolution()) {
            return true;
        } else {
            return false;
        }
    }

    public Move genMove(String color) {
        String t = "chooseMove(0," + color + ",I,J)";
        Query q = new Query(t);
        if (q.hasSolution()) {
            Hashtable h = q.oneSolution();
            Move m = new Move(new Integer(h.get("I").toString()).intValue(), new Integer(h.get("J").toString()).intValue(), color);
            return m;
        } else {
            Move pass = new Move(0, 0, color);
            return pass;
        }
    }

    public boolean boardsize(int boardsize) {
        String t = "hasSquareSize(X," + boardsize + ")";
        Query q = new Query(t);
        if (q.hasSolution()) {
            Hashtable h = q.oneSolution();
            String t2 = "retractall(initialConfiguration(X))";
            q = new Query(t2);
            q.hasSolution();
            q = new Query("assert(initialConfiguration(" + h.get("X") + "))");
            q.hasSolution();
            return true;
        } else {
            return false;
        }
    }
}
