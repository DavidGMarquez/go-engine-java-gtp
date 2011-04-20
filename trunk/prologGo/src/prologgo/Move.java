package prologgo;

/**
 *
 * @author Cayette
 */
public class Move {

    private int rank;
    private int file;
    private String color;

    public Move() {
    }

    /**
     * @return the rank
     */
    public int getRank() {
        return rank;
    }

    /**
     * @param rank the rank to set
     */
    public void setRank(int rank) {
        this.rank = rank;
    }

    /**
     * @return the file
     */
    public int getFile() {
        return file;
    }

    /**
     * @param file the file to set
     */
    public void setFile(char file) {
        this.file = file;
    }

    /**
     * @return the color
     */
    public String getColor() {
        return color;
    }

    /**
     * @param color the color to set
     */
    public void setColor(String color) {
        this.color = color;
    }

    public Move(int rank, int file, String color) {
        this.rank = rank;
        this.file = file;
        this.color = color;
    }

    public String toPlayString() {
        char f = this.fileToLetter();
        String a = (f + "").toUpperCase();
        String c = this.color.substring(0, 1).toUpperCase();
        return "play " + c + " " + a + this.rank;
    }

    public String toGenString() {
        char f = this.fileToLetter();
        String a = (f + "").toUpperCase();
        if (!this.isPass()) {
            return a + this.rank;
        } else {
            return "PASS";
        }
    }

    public boolean isPass() {
        return (file == 0 && rank == 0);
    }

    public char fileToLetter() {
        char c;
        if (this.file < 9) {
            c = (char) (this.file + 96);
        } else {
            c = (char) (this.file + 97);
        }
        return c;
    }
}
