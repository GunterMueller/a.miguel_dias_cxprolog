package prolog ;

/*
 *   This file is part of the CxProlog system

 *   JavaList.java
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
 
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class JavaList extends JavaGraphicalObject {
	private JList<String> list;
	private JavaGraphicalWindow father;

	public JavaList(JavaGraphicalWindow window, int x, int y, String[] items) {
		super();
		list = new JList<String>(items);
		father = window;
		window.getContentPane().add(list);
		Insets i = window.getInsets();
		Dimension listSize = list.getPreferredSize();
		list.setBounds(x + i.left,y + i.top,listSize.width,listSize.height);
		list.setSelectedIndex(0);
	}
	
	public void draw(Graphics gfx) {}
	
	public void delete() {
		super.delete();
		//System.out.println("delete do botao");
		father.remove(list);
	}
	
	public String getSelectedItem() {
		return (String)list.getSelectedValue();
	} 
}
