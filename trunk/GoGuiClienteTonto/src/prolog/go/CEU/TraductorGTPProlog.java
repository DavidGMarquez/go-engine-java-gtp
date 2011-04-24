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

    // Genera un movimiento. En caso de ser un "pass", moveToGoPoint devolvera null
    public GoPoint genMove(GoColor goColor) throws GtpError {
        if (goColor.equals(GoColor.BLACK)) {
            Move move = queries.genMove("black");
            return moveToGoPoint(move);
        } else {
            if (goColor.equals(GoColor.WHITE)) {
                Move move = queries.genMove("white");
                return moveToGoPoint(move);
            }
        }
        throw new GtpError("No such color");
    }

    public void addPiece(GoPoint point, GoColor goColor) throws GtpError {
        Move move = null;
        move = createMove(point, goColor);
        if (move != null) {
            if(!queries.addPiece(move)){
                throw new GtpError("Illegal move");
            }
        } else {
            throw new GtpError("No such color");
        }
    }

    public Move createMove(GoPoint point, GoColor gocolor) {
        Move m = new Move();
        m.setRank(point.getX()+1);
        m.setFile(point.getY()+1);
        if (gocolor.equals(GoColor.BLACK)) {
            m.setColor("black");
        } else {
            m.setColor("white");
        }
        return m;
    }

    // Convierte un Move en un GoPoint. Si el Move es de pasar (0,0), devuelve null
    public GoPoint moveToGoPoint(Move m) {
        if (m.isPass()) {
            return null;
        } else {
            return GoPoint.get(m.getRank()-1, m.getFile()-1);
        }
    }
}
