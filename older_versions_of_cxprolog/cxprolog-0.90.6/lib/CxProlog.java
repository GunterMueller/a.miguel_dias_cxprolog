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


public class CxProlog {

/* GUI EVENTS */

	private static java.util.List queue = new java.util.Vector() ;

	private synchronized static void WaitEvent() {
		for(;;)
			try {
				CxProlog.class.wait() ;
				break ;
			}
			catch( Exception e ) { }
	}

	private synchronized static void NotifyEvent() {
		try {
			CxProlog.class.notify() ;
		}
		catch( Exception e ) { /* cannot happen */ }
	}

	public static void DiscardEvents() {
		queue.clear() ;
	}

	public static int HowManyEvents() {
		return queue.size() ;
	}

	public static String GetNextEvent() {
		if( queue.size() == 0 ) WaitEvent() ;
		return (String)queue.remove(0) ;
	}

	public static void PostEvent(int i) {
		PostEvent(Integer.toString(i)) ;
		if( queue.size() == 1 ) NotifyEvent() ;
	}

	public static void PostEvent(String s) {
		queue.add(s) ;
		if( queue.size() == 1 ) NotifyEvent() ;
	}


/* MORE STUFF HERE */

	/* nothing as yet */

}
