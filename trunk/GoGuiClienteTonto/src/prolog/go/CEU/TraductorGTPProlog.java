/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package prolog.go.CEU;

import jpl.Query;
import net.sf.gogui.go.GoColor;
import net.sf.gogui.go.GoPoint;
import net.sf.gogui.gtp.GtpError;

/**
 *
 * @author USUARIO
 */
public class TraductorGTPProlog {

    String file;
    int defaultSize;
    Queries queries = new Queries();

    public TraductorGTPProlog(String file, int defaultSize) {
        this.file = file;
        this.defaultSize = defaultSize;
        String t1 = "consult('" + file + "')";
        Query q1 = new Query(t1);
        System.out.println(t1 + " " + (q1.hasSolution() ? "succeeded" : "failed"));
        q1.close();
        queries.newGame();
        if (queries.boardsize(defaultSize)) {
            System.out.println("Boardsize changed");
        } else {
            System.out.println("Illegal boardsize");
        }
    }

    public void clearBoard() {
        queries.newGame();
    }

    public void boardSize(int newSize) {
        queries.boardsize(newSize);
    }

    public GoPoint genMove(GoColor goColor) throws GtpError {
        if (goColor.equals(GoColor.BLACK)) {
            Move move = queries.genMove("black");
            //@duda aqui tengo mis dudas del file y rank
            //Para que pasara debería devolver un GoPoint null
            return GoPoint.get(move.getRank(), move.getFile());
        } else {
            if (goColor.equals(GoColor.WHITE)) {
                Move move = queries.genMove("white");
                return GoPoint.get(move.getRank(), move.getFile());
            }
        }
        throw new GtpError("No such color");
    }

    public void addPiece(GoPoint point, GoColor goColor) throws GtpError {
        Move move = null;
        if (goColor.equals(GoColor.BLACK)) {
            move = new Move(point.getX(), point.getY(), "black");
        } else {
            if (goColor.equals(GoColor.WHITE)) {
                move = new Move(point.getX(), point.getY(), "white");
            }
        }
        if (move != null) {
            queries.addPiece(move);
        } else {
            throw new GtpError("No such color");
        }
    }
}


