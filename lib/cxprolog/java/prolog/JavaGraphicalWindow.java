package prolog ;

/*
 *   This file is part of the CxProlog system

 *   JavaGraphicalWindow.java
 *   by Henrique Oliveira, A.Miguel Dias - 2006/04/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 Henrique Oliveira, A.Miguel Dias, CITI, DI/FCT/UNL

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

/*
 Changelog:
	2008/01/18 - Made thread-safe by amd
*/

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.util.*;

public class JavaGraphicalWindow extends JFrame {
	
	private java.util.List<JavaGraphicalObject> graphicalObjects;
	/*
	public void printObjects() {
		System.out.println("objectos:");
		Iterator<JavaGraphicalObject> i = graphicalObjects.iterator();
		
		while(i.hasNext())
			System.out.println(i.next().getClass());
	}
	*/

	public JavaGraphicalWindow(String title, int x0, int y0, int width, int height) {
		super();
		graphicalObjects = new Vector<JavaGraphicalObject>() ;

		setTitle(title);
		setLocation(x0, y0);
		setSize(new Dimension(width, height));
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE) ;
		setLayout(null);
		
		addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				Integer x = new Integer(e.getX());
				Integer y = new Integer(e.getY());
				Prolog.PostEvent("event", JavaGraphicalWindow.this,
											null, "mouse_down", x, null, y);
			}			
		});
		
		super.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				Prolog.PostEvent("event", JavaGraphicalWindow.this,
											null, "close_window");
			}
		});
		
		Prolog.GuiCall(new Runnable() {
			public void run() {
				setVisible(true);
				toFront();
   			}
		}) ;
	}

	public void paint(Graphics g) {
		//System.out.print("paint - ");
		//printObjects();
		super.paint(g);
		for (int i = 0; i < graphicalObjects.size(); i++)
			graphicalObjects.get(i).draw(g);
	}
	
	
	/* METHODS FOR CREATING GRAPHICAL OBJECTS */
	public JavaGraphicalObject gfxPutPixel(int x, int y, int r, int g, int b) {
		Graphics gfx = super.getGraphics();		
		JavaGraphicalObject pixel = new JavaPixel(this,x, y, r, g, b);
		graphicalObjects.add(pixel);
		return pixel;
	}
	
	public JavaGraphicalObject gfxLine(int x0, int y0, int x1, int y1, int r, int g, int b) {
		Graphics gfx = super.getGraphics();
		JavaGraphicalObject line = new JavaLine(this,x0,y0,x1,y1,r,g,b);
		graphicalObjects.add(line);
		return line;
	}

	public JavaGraphicalObject gfxCircle(int x, int y, int radius, int r, int g, int b) {
		Graphics gfx = super.getGraphics();
		JavaGraphicalObject circle = new JavaCircle(this,x,y,radius,r,g,b,false);
		graphicalObjects.add(circle);
		return circle;
	}

	public JavaGraphicalObject gfxRectangle(int x, int y, int width, int height, int r, int g, int b) {
		Graphics gfx = super.getGraphics();
		JavaGraphicalObject rectangle = new JavaRectangle(this,x,y,width,height,r,g,b,false);
		graphicalObjects.add(rectangle);
		return rectangle;
	}
	
	public JavaGraphicalObject gfxCircleFilled(int x, int y, int radius, int r, int g, int b) {
		Graphics gfx = super.getGraphics();
		JavaGraphicalObject circle = new JavaCircle(this,x,y,radius,r,g,b,true);
		graphicalObjects.add(circle);
		return circle;
	}

	public JavaGraphicalObject gfxRectangleFilled(int x, int y, int width, int height, int r, int g, int b) {
		Graphics gfx = super.getGraphics();
		JavaGraphicalObject rectangle = new JavaRectangle(this,x,y,width,height,r,g,b,true);
		graphicalObjects.add(rectangle);
		return rectangle;
	}

	public JavaGraphicalObject gfxDrawText(String txt, int x, int y) {
		Graphics gfx = super.getGraphics();
		JavaGraphicalObject text = new JavaText(this,txt,x,y);
		graphicalObjects.add(text);
		return text;
	}
	
	public JavaGraphicalObject gfxButton(String txt, int x, int y) {
		JavaGraphicalObject button = new JavaButton(this, x, y, txt);
		graphicalObjects.add(button);
		return button;
	}
	
	public JavaGraphicalObject gfxList(String[] items, int x, int y) {
		JavaGraphicalObject list = new JavaList(this, x, y, items);
		graphicalObjects.add(list);
		return list;
	}
	
	public void gfxDeleteObject(JavaGraphicalObject object) {
		object.delete();
		graphicalObjects.remove(object);
		//paint(super.getGraphics());	
	}
	
	public void gfxClear() {
		for (int i = 0; i < graphicalObjects.size(); i++)
			graphicalObjects.get(i).delete();
		graphicalObjects.clear();
			
		Graphics gfx = super.getGraphics();
		gfx.clearRect(0, 0, super.getWidth(), super.getHeight());	
	}
	
	public void gfxClose() {
		gfxClear();
		super.dispose();
	}
		
	public static void main(String args[]) throws Exception {
		
		JavaGraphicalWindow w = new JavaGraphicalWindow("ola", 100, 100, 500, 500);
		/*
		w.gfxLine(100,100,400,400,255,0,0);
		w.gfxCircle(250,250,200,0,255,0);
		w.gfxCircleFilled(250,250,100,0,255,0);
		w.gfxRectangle(300,200,100,50,0,0,255);
		w.gfxDrawText("ola",100,200);
		*/
		//w.printObjects();
		w.gfxButton("ola",300,400);
		//w.printObjects();
		
		w.gfxClear();
		//w.printObjects();
		String[] items = {"ola1","ola2","ola3"};
		
		JavaList l = (JavaList)w.gfxList(items, 100, 100);
		
		while(true) {
			Thread.sleep(1000);
			System.out.println(l.getSelectedItem());
		}
		
		/*		
		Thread.sleep(2000);
		w.gfxClear();
		Thread.sleep(2000);
		w.gfxClose();
		*/
	}
}
