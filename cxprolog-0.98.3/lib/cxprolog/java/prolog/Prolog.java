package prolog ;

/*
 *   This file is part of the CxProlog system

 *   Prolog.java
 *   by A.Miguel Dias - 2004/08/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL

 *   it under the terms of the GNU General Public License as published by
 *   CxProlog is free software; you can redistribute it and/or modify
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

public class Prolog { /* GUI EVENTS: At least, Java 5.0 is required */
	static boolean started = false;

/* Stuff to use when starting from the Java side */
	public static native void touch();
	public static native void hello();
	public static native void SetupProlog();
	
	public static void StartProlog(String lib) {
		if( started ) {		
			System.err.println("[prolog.Prolog.StartProlog] " + "called twice");
			System.exit(1);		
		}
		started = true;
		try {
			System.loadLibrary(lib);
		} catch( UnsatisfiedLinkError e ) {
			System.err.println("[prolog.Prolog.StartProlog] " + e);
			System.exit(1);
    	}
 		try {
			touch();
		} catch( UnsatisfiedLinkError e ) {
			System.err.println("[prolog.Prolog.StartProlog] "
				+ "Java is disabled in the CxProlog dynamic library. "
				+ "Recompile: make lib JAVA=y");
			System.exit(1);
    	}
		SetupProlog();
	}
	
	public static void StartProlog() {
		StartProlog("cxprolog");
	}

	public static void main(String[] args) {
		StartProlog();
		System.out.println("---") ;
		TestCallProlog();
		System.out.println("---") ;
		TestIVars();
		System.out.println("---") ;
	}
/* End of stuff to use when starting from the Java side */


/* EVENTS */

/* This is the event queue */
	private static java.util.List<Object []> evQueue =
								new java.util.Vector<Object []>() ;

/* The sequence of arguments of PostEvent represents a serialized n-ary
   tree that will be converted to a proper Prolog term, later when Prolog
   gets it via GetNextEvent. The serialization of the tree is based on the
   following simples recursive rules:
		1. First the root;
		2. Next the subtrees, left-to-right;
		3. Finally, the constant "null" marking the end of the subtrees.
	The final "null"s of the complete sequence may be omitted. All inner
	nodes must be strings and will be converted to Prolog functors. All
	leaves must be non-null objects and will be converted to appropriate
	Prolog values; in particular, any leave of type Object[] is converted
	to a Prolog list (and its components are also recursivelly converted).
	Examples:
		Prolog.PostEvent("event", this, null, "tick", 54) ;
					------> event(1'JOBJ_407f0e40,tick(54))
		Prolog.PostEvent("event", this, null, "ticks", new Object[]{1,2,3}) ;
					------> event(1'JOBJ_407f0e40,ticks([1,2,3]))
*/
	public static synchronized void PostEvent(Object... elems) {
		evQueue.add(elems) ;
		if( evQueue.size() == 1 ) {
			NotifyEvent() ;
			Prolog.class.notify() ;
		}
	}
	public static synchronized Object[] GetNextEvent() {
		while( evQueue.size() == 0 ) {
			try {
				Prolog.class.wait() ;
			} catch( Exception e ) { }
		}
		return evQueue.remove(0) ;
	}
	public static synchronized void DiscardEvents() {
		evQueue.clear() ;
	}
	public static int HowManyEvents() {
		return evQueue.size() ;
	}
	private native static void NotifyEvent() ;


/* CALL PROLOG from Java */
	public native static synchronized boolean CallProlog(String term) ;
	public native static synchronized boolean CallProlog(Object... term) ;
	public static void TestCallProlog()
	{
		CallProlog("writeln('Testing CallProlog...')") ;
		CallProlog("zjava");
		try {
			CallProlog("writeln('Testing PrologException:'), see(1), writeln(ole)") ;
		} catch( Exception e ) {
			System.out.println("    " + e.getClass().getCanonicalName() + " occurred") ;
			System.out.print("    " + e.getMessage()) ;
			if( e instanceof PrologException )
				System.out.println("    " + ((PrologException)e).getExceptionTerm()[0]) ;
		}
		CallProlog("writeln('Test done!')") ;
	/* TEST: java_call('prolog/Prolog', 'TestCallProlog:()V', [], R). */
	}


/* IVARS */
	public native static synchronized Object[] IVarGet(String name) ;
	public native static synchronized void IVarSet(String name, String term) ;
	public native static synchronized void IVarSet(String name, Object... term) ;
	public static void TestIVars()
	{
		java.util.List<String> v = new java.util.Vector<String>(2);
		v.add("ola");
		v.add("ole");

		System.out.println("Testing IVARS:") ;		
		CallProlog("ivars") ;
		IVarSet("aaa", "aaa(4)");
		IVarSet("bbb", "bbb", "ccc");
		IVarSet("ccc", "bbb", "ccc", "ddd", null, 123, null, 123.123, null, v);
		CallProlog("ivars") ;

		Object[] arr = IVarGet("ccc");
		System.out.print("    ") ;
		for(Object a : arr)
			System.out.print("" + a + ", ") ;
		System.out.println() ;
		
		System.out.println("Test done!") ;		
	}


/* GUI */
	public static void GuiCall(Runnable r)
	{
		javax.swing.SwingUtilities.invokeLater(r) ;
	}
	public static void GuiCallWait(Runnable r)
	{
		try {
			javax.swing.SwingUtilities.invokeAndWait(r) ;
		} catch (Exception e) {
			e.printStackTrace() ;
		}
	}
	public static boolean sleep(long milisec) {
		try {
			Thread.sleep(milisec) ;
			return true ;
		} catch(InterruptedException e) {
			Thread.currentThread().interrupt();
		}
		return false;
	}

/* UTIL */
	public static void Identify()
	{
		System.out.println("prolog.Prolog");
	}
	public static void PrintThreadId(String s)
	{
		Info("Java thread " + s +
			", id = " + Thread.currentThread().getId()) ;
	}
	public static boolean PreValidateTerm(String term)
	{
		return true;
	}
	public static void CheckThread()
	{
		if( !javax.swing.SwingUtilities.isEventDispatchThread() )
			Warning("Graphic op. OUTSIDE the GUI thread?!?!") ;	
		else			
			Info("Graphic op. inside the GUI thread") ;	
	}
	public static String getClasspath()
	{
		return System.getProperty("java.class.path")	;
	}
	public static Object Null()
	{
		return null	;
	/* TEST: java_call('prolog/Prolog', 'Null:()Ljava/lang/Object;', [], R) */
	}
	public static void genException() throws java.text.ParseException
	{
		throw new java.text.ParseException("just a test", 21056) ;
	}
	public static void testMixArray(Object... arr)
	{
		for(Object a : arr)
			System.out.print("" + a + ", ") ;
		if( arr.length > 0 )
			System.out.println() ;
	}
	public static Object objId(Object obj)
	{
		return obj ;
	}
	public static Object someObj()
	{
		return new int[10] ;
	}
// java_call('prolog/Prolog', 'testMixArray:([Ljava/lang/Object;)V', [], Res).

	public native static synchronized void Info(String s) ;
	public native static synchronized void Warning(String s) ;
	public native static synchronized void Error(String s) ;
	
/* INIT */
	public static void Init() {
		started = true;
		// Set specific platform look and feel		
 		try {
			// the following line crashes in Java 6.0/Ubuntu
   			// UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()) ;
   			// System.out.println("running prolog.Prolog.Init");
		} catch( Exception e ) {}
	}

/* COROUTINING SWING EVENT QUEUE */
	private static class CoroutiningEventQueue extends java.awt.EventQueue {
		private static CoroutiningEventQueue singleton = null;
		private static java.awt.AWTEvent runABitEvent = null;	
		private static boolean coroutiningOn = false;
		protected void dispatchEvent(java.awt.AWTEvent event) {
			if( event == runABitEvent ) {
				if( coroutiningOn ) {
					if( !coroutiningRunABit("mythread") ) {
						sleep(300);
					//	System.out.println("WAIT");
					}	
					else System.out.println("****    INPUT");
					System.out.print(Prolog.coroutiningOutputText("mythread"));
					singleton.postEvent(runABitEvent);
				}
			}
			else
				super.dispatchEvent(event);
		}
		public static void start() {
			if( singleton == null ) {
				coroutiningOn = false ;
				singleton = new CoroutiningEventQueue() ;
				runABitEvent = new java.awt.event.ActionEvent(singleton, 0, "");
			}
			if( !coroutiningOn )
				java.awt.Toolkit.getDefaultToolkit().getSystemEventQueue().push(singleton);
			coroutiningOn = true;
			singleton.postEvent(runABitEvent);
		}
		public static void stop() {
			boolean c = coroutiningOn;
			coroutiningOn = false ;
			if( c )
				singleton.pop();
		}		
	}
	public static void coroutiningStart() {
		// System.out.println("coroutiningStart");
		CoroutiningEventQueue.start();
	}
	public static void coroutiningStop() {
		// System.out.println("coroutiningStop");
		CoroutiningEventQueue.stop();
	}
	public static boolean coroutiningRunABit(String coroutine) {
		// System.out.println("coroutiningRunABit");
		try {
			Prolog.CallProlog("thread_run_a_bit", coroutine) ;
		} catch (Throwable t) {
			System.err.print(t.getMessage());
		}
		return !coroutiningWaitingInput(coroutine) ;
	}
	
	public static native void coroutiningInput(String coroutine, String text) ;
	public static native String coroutiningOutputText(String coroutine) ;
	public static void coroutiningSetOutputHandler(OutputHandler handler) {}
	private static native boolean coroutiningWaitingInput(String coroutine) ;
	public static native void coroutiningInterrupt(String coroutine);
	public static native boolean coroutiningIsWaitingAtTopLevel(String coroutine);
	public static native void coroutiningSetAutoOutput(String coroutine, boolean ao);

	public static OutputHandler stdOut = (String text) -> { System.out.println(text); };

/* MORE STUFF HERE */

	/* nothing as yet */

}
