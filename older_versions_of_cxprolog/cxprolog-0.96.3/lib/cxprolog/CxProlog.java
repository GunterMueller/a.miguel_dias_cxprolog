/*
 *   This file is part of the CxProlog system

 *   CxProlog.java
 *   by A.Miguel Dias - 2004/08/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

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

import javax.swing.* ;

public class CxProlog { /* GUI EVENTS: At least, Java 5.0 is required */

/* This is the event queue */
	private static java.util.List<Object []> evQueue =
								new java.util.Vector<Object []>() ;

/* The sequence of arguments of PostEvent represents a serialized n-ary
   tree that will be converted to a proper Prolog term, later when Prolog
   gets it via GetNextEvent. The serialization of the tree is based on the
   following simples recursive rules:
		1. First the root;
		2. Next the subtrees, left-to-right;
		3. Finally, the constant "null" marking the end.
	The final "null"s of the complete sequence may be omitted. All inner
	nodes must be strings and will be converted to Prolog functors. All
	leaves must be non-null objects and will be converted to appropriate
	Prolog values; in particular, any leave of type Object[] is converted
	to a Prolog list (and its components are also recursivelly converted).
	Examples:
		CxProlog.PostEvent("event", this, null, "tick", 54) ;
					------> event(1'JOBJ_407f0e40,tick(54))
		CxProlog.PostEvent("event", this, null, "ticks", new Object[]{1,2,3}) ;
					------> event(1'JOBJ_407f0e40,ticks([1,2,3]))
*/
	public static synchronized void PostEvent(Object... elems) {
		evQueue.add(elems) ;
		if( evQueue.size() == 1 ) {
			NotifyProlog() ;
			try {	 	// NotifyEvent
				CxProlog.class.notify() ;
			}
			catch( Exception e ) { /* cannot happen */ }
		}
	}
 
	public static synchronized Object [] GetNextEvent() {
		if( evQueue.size() == 0 ) {
			for(;;)		 // WaitEvent
				try {
					CxProlog.class.wait() ;
					break ;
				}
				catch( Exception e ) { }
		}
		return evQueue.remove(0) ;
	}

	public static synchronized void DiscardEvents() {
		evQueue.clear() ;
	}

	public static int HowManyEvents() {
		return evQueue.size() ;
	}

	private native static void NotifyProlog() ;

	private native static boolean CallPrologStr(String s) ;

	public static void Init() {
		// Set specific platform look and feel		
 		try {
			// crashes in Java 6.0/Ubuntu
   			// UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()) ;
		} catch( Exception e ) {}
	}


/* MORE STUFF HERE */

	/* nothing as yet */

}
