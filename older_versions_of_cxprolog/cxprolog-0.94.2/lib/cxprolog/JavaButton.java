/*
 *   This file is part of the CxProlog system

 *   JavaButton.java
 *   by Hnrique Oliveira, A.Miguel Dias - 2006/04/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 Henrique Oliveira, A.Miguel Dias, CITI, DI/FCT/UNL

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

public class JavaButton extends JavaGraphicalObject {
	private int x;
	private int y;
	private String txt;
	private JButton button;
	private JavaGraphicalWindow window;
	
	public JavaButton(JavaGraphicalWindow window, int x, int y, String txt) {
		super();
		this.x = x;
		this.y = y;
		this.txt = txt;
		this.button = new JButton(txt);
		this.window = window;
		
		final JavaGraphicalWindow w = window;
		//final String buttonName = txt;
		final JavaGraphicalObject b = this;
		button.addActionListener(new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				CxProlog.PostEvent("event", w, null, "button", b);
			}
		});
		window.getContentPane().add(button);
		Insets i = window.getInsets();
		Dimension buttonSize = button.getPreferredSize();
		button.setBounds(x + i.left,y + i.top,buttonSize.width,buttonSize.height);
		window.setVisible(true);
	}
	
	public void draw(Graphics gfx) {}
	
	public void delete() {
		super.delete();
		//System.out.println("delete do botao");
		window.remove(button);
	}
}
