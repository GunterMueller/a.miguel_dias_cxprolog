package prolog ;

/*
 *   This file is part of the CxProlog system

 *   JavaCircle.java
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

public class JavaCircle extends JavaGraphicalObject {
	private int x;
	private int y;
	private int radius;
	Color color ;
	private boolean filled;
	
	public JavaCircle(JFrame father, int x, int y, int radius, int r, int g, int b, boolean filled) {
		super();
		this.x = x;
		this.y = y;
		this.radius = radius;
		color = new Color(r,g,b) ;
		this.filled = filled;
		father.repaint() ;
		//father.repaint(x - radius, y - radius, radius * 2 + 1, radius * 2 + 1) ;
	}

	public void draw(Graphics gfx) {
		gfx.setColor(color);
		int upperLeftX = x - radius;
		int upperLeftY = y - radius;
		int width = radius * 2;
		int height = radius * 2;
		if (filled)
			gfx.fillOval(upperLeftX,upperLeftY,width,height);
		else
			gfx.drawOval(upperLeftX,upperLeftY,width,height);

	}
}
