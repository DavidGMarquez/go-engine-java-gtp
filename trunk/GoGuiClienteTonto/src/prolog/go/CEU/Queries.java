package prolog.go.CEU;

import java.util.Hashtable;
import jpl.*;

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
        String t = "placeStoneWithoutMove(0," + color + "," + rank + "," + file + ")";
        Query q = new Query(t);
        if (q.hasSolution()) {
            return true;
        } else {
            return false;
        }
    }

    public boolean addPiece(Move m) {
        String t = "placeStoneWithoutMove(0," + m.getColor() + "," + m.getRank() + "," + m.getFile() + ")";
        Query q = new Query(t);
        if (q.hasSolution()) {
            return true;
        } else {
            return false;
        }
    }

    public Move genMove(String color) {
        String t = "chooseMoves(0," + color + ",L)";
        Query q = new Query(t);
        if (q.hasSolution()) {
            Hashtable h = q.oneSolution();
            Term[] moves=((Term)h.get("L")).toTermArray();
            java.util.Random r=new java.util.Random();
            int random_move=r.nextInt(moves.length);
            Term move=moves[random_move];
            Move m = new Move(move.args()[0].intValue(), move.args()[1].intValue(), color);
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
