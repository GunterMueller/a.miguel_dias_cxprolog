package ticktack2 ;

import java.beans.* ;
import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import javax.swing.event.* ;

import prolog.* ;

class MyEventQueue extends EventQueue {
	private static MyEventQueue singleton = null;
	private static AWTEvent runABitEvent = null;	
	protected void dispatchEvent(AWTEvent event) {
		if( event == runABitEvent )
			Prolog.PostEvent("event", "noframe", null, "runabit") ;
		else
			super.dispatchEvent(event);
	}
	public static void start() {
		if( singleton == null )
			singleton = new MyEventQueue() ;
		Toolkit.getDefaultToolkit().getSystemEventQueue().push(singleton);
		runABitEvent = new ActionEvent(singleton, 0, "");
		askABitMore();
	}	
	public static void stop() {
		System.out.println("OUT");
		if( singleton == null ) return ;
		singleton.pop();
	}		
	public static void askABitMore() {
		System.out.println("MORE");
		if( singleton == null ) return ;
		singleton.postEvent(runABitEvent);
	}		
}
