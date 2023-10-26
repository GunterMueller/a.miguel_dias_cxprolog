import java.beans.* ;
import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import javax.swing.event.* ;

public class TickTack extends JComponent implements Runnable {
    private int ticks ;
    private int id ;
	private static int n = 0 ;

    public TickTack() {
        super() ;
        ticks = 0 ;
		id = n++ ;
        (new Thread(this)).start() ;
    }

    public int getTicks() {
        return ticks ;
    }

    public void run() {
		CxProlog.PrintThreadId("TickTack") ;
        for(;;) {
            try { Thread.sleep(1000) ; } catch (InterruptedException e) { }
            ticks++ ;
            firePropertyChange("ticks", null, null);
        }
    }
}
