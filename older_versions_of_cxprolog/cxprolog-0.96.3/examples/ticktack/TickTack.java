import java.beans.* ;
import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import javax.swing.event.* ;

public class TickTack extends JComponent implements Runnable {
    private int ticks ;

    public TickTack() {
        super() ;
        ticks = 0 ;
        (new Thread(this)).start() ;
    }

    public int getTicks() {
        return ticks ;
    }

    public void run() {
        for(;;) {
            try { Thread.sleep(1000) ; } catch (InterruptedException e) { }
            ticks++ ;
            firePropertyChange("ticks", null, null);
        }
    }
}
