package prolog ;

/*
 *   This file is part of the CxProlog system

 *   JavaLine.java
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

public class JavaLine extends JavaGraphicalObject {
	private int x0;
	private int y0;
	private int x1;
	private int y1;
	Color color ;
	
	public JavaLine(JFrame father, int x0, int y0, int x1, int y1, int r, int g, int b) {
		super();
		this.x0 = x0;
		this.y0 = y0;
		this.x1 = x1;
		this.y1 = y1;
		color = new Color(r,g,b) ;
		father.repaint() ;
		//father.repaint(x0, y0, x1 - x0 + 1, y1 - y0 + 1) ;
	}
	
	public void draw(Graphics gfx) {
		gfx.setColor(color);
		gfx.drawLine(x0,y0,x1,y1);
	}
}
