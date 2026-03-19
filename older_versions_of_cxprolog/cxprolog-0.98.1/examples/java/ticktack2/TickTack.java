package ticktack2 ;

import java.beans.* ;
import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import javax.swing.event.* ;

import prolog.* ;

public class TickTack extends JComponent implements Runnable {
	private Thread t ;
    private int ticks ;
    private int id ;
	private static int n = 0 ;

    public TickTack() {
        super() ;
        ticks = 0 ;
		id = n++ ;
		t = new Thread(this);
		t.start();
    }

    public int getTicks() {
        return ticks ;
    }

    public void run() {
        for(;;) {
            try { Thread.sleep(1000) ; }
			catch (InterruptedException e) { break; }
            ticks++ ;
            firePropertyChange("ticks", null, null);
        }
    }
	public void stop() {
		t.interrupt();
	}
	public static void main(String[] args) {
		System.out.println("TickTack") ;
	}
}
