package prolog ;

/*
 *   This file is part of the CxProlog system

 *   JavaRectangle.java
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

public class JavaRectangle extends JavaGraphicalObject {
	private int x;
	private int y;
	private int width;
	private int height;
	Color color ;
	private boolean filled;
	
	public JavaRectangle(JFrame father, int x, int y, int width, int height, int r, int g, int b, boolean filled) {
		super();
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
		color = new Color(r,g,b) ;
		this.filled = filled;
		father.repaint() ;
		//father.repaint(x, y, width + 1, width + 1) ;
	}
	
	public void draw(Graphics gfx) {
		gfx.setColor(color);
		if (filled)
			gfx.fillRect(x,y,width,height);
		else
			gfx.drawRect(x,y,width,height);
	}
}
