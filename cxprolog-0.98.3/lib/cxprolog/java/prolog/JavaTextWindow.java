package prolog ;

/*
 *   This file is part of the CxProlog system

 *   JavaTextWindow.java
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
import javax.swing.event.*;
import javax.swing.undo.*;
import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.util.*;

public class JavaTextWindow extends JFrame {
	private JTextPane editor;
	private String filename;
	private UndoManager undoManager;


	public JavaTextWindow(String title, int x0, int y0, 
		int width, int height, String[][] menus) {
		
		super(title);
		super.setLocation(x0, y0);
		super.setSize(new Dimension(width, height));
		super.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
				
		// Build menus
		JMenuBar menuBar = new JMenuBar();
		
		int mnemonicIndex;
		
		for (int i = 0; i < menus.length; i++) {
			
			String menuText = menus[i][0];
			mnemonicIndex = menuText.indexOf('&');
			if (mnemonicIndex != -1)
				menuText = menuText.substring(0, mnemonicIndex) + 
					menuText.substring(mnemonicIndex + 1, menuText.length());
			
			JMenu menu = new JMenu(menuText);

			if (mnemonicIndex != -1) {
				menu.setMnemonic(Character.getNumericValue(menuText.charAt(mnemonicIndex)) + 55);
			}
			
			for (int j = 1; j < menus[i].length; j++) {
				if(menus[i][j].equals("|"))
					menu.addSeparator();
				else {
					String menuItemText = menus[i][j];
					mnemonicIndex = menuItemText.indexOf('&');
					if (mnemonicIndex != -1)
						menuItemText = menuItemText.substring(0, mnemonicIndex) + 
							menuItemText.substring(mnemonicIndex + 1, menuItemText.length());
					
					final JMenuItem menuItem = new JMenuItem(menuItemText);
					
					if (mnemonicIndex != -1) {
						menuItem.setMnemonic(Character.getNumericValue(menuItemText.charAt(mnemonicIndex)) + 55);
					}
					
					final JavaTextWindow w = this;
					menuItem.addActionListener(new AbstractAction() {
						public void actionPerformed(ActionEvent e) {
							String menuItemText = menuItem.getText();
							Prolog.PostEvent("event", w, null, "usermenu", menuItemText);							
						}
					});
					
					menu.add(menuItem);
				}
			}
			menuBar.add(menu);
		}
		super.setJMenuBar(menuBar);
		
		// Create editor
		editor = new JTextPane();
		
		// Create scroll bar
		JScrollPane scrollPane = new JScrollPane(editor);
		super.add(scrollPane);
		
		// Add undo manager
		undoManager = new UndoManager();
		editor.getDocument().addUndoableEditListener(new UndoableEditListener() {
    		public void undoableEditHappened(UndoableEditEvent e) {
        		undoManager.addEdit(e.getEdit());	
    		}
		});
		//editor.getDocument().addUndoableEditListener(undoManager);

		// Build popup menu
		editor.setComponentPopupMenu(buildPopupMenu());
		
		final JavaTextWindow w = this;
		super.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				Prolog.PostEvent("event", w, null, "close_window");
      }
		});
					
		super.setVisible(true);
		super.toFront();
	}
	
	private JPopupMenu buildPopupMenu() {
		JPopupMenu popupMenu = new JPopupMenu();
		
		final JMenuItem undoItem = new JMenuItem("Undo");
		undoItem.addActionListener(new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				popupUndo();										
			}
		});
		popupMenu.add(undoItem);
		
		popupMenu.addSeparator();
		
		final JMenuItem cutItem = new JMenuItem("Cut");
		cutItem.addActionListener(new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				popupCut();										
			}
		});
		popupMenu.add(cutItem);
		
		final JMenuItem copyItem = new JMenuItem("Copy");
		copyItem.addActionListener(new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				popupCopy();										
			}
		});
		popupMenu.add(copyItem);
		
		final JMenuItem pasteItem = new JMenuItem("Paste");
		pasteItem.addActionListener(new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				popupPaste();										
			}
		});
		popupMenu.add(pasteItem);
		
		final JMenuItem deleteItem = new JMenuItem("Delete");
		deleteItem.addActionListener(new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				popupDelete();										
			}
		});
		popupMenu.add(deleteItem);
		
		popupMenu.addSeparator();
		
		final JMenuItem selectAllItem = new JMenuItem("Select All");
		selectAllItem.addActionListener(new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				popupSelectAll();										
			}
		});
		popupMenu.add(selectAllItem);
		
		return popupMenu;
	}
	
	/* Handlers for right click popup menu operations */
	
	private void popupUndo() {
		try {
			undoManager.undo();
    	} catch (CannotUndoException e) {
    		System.err.println("Unable to undo " + e.getMessage());
    	}
	}
	
	private void popupCut() {
		editor.cut();
	}
	
	private void popupCopy() {
		editor.copy();
	}
		
	private void popupPaste() {
		editor.paste();
	}
	
	private void popupDelete() {
		int start = editor.getSelectionStart();
		int end = editor.getSelectionEnd();
		txtReplace(start, end, "");
	}
	
	private void popupSelectAll() {
		editor.selectAll();
	}
		
	/* Operations available from within CxProlog */
	
	public String txtGetFilePath() {
		if (filename == null)
			return "";
		else
			return filename;
	}
	
	public int txtOpenFile() {
		filename = JavaGeneral.fileChooserSimpleMsg("Open");
		if (filename == null)
			return -1;
		try {
			FileReader reader = new FileReader(filename);
			editor.read(reader, null);
		} catch (FileNotFoundException e) {
			System.err.println("Couldn't find file " + filename + "while opening");
		}	
		catch (IOException e) {
			System.err.println("Couldn't read file " + filename);
		}
		
		// Add new undo manager for new document
		undoManager = new UndoManager();
		editor.getDocument().addUndoableEditListener(new UndoableEditListener() {
    		public void undoableEditHappened(UndoableEditEvent e) {
        	undoManager.addEdit(e.getEdit());	
    		}
		});
		
		this.setTitle("Edit - " + filename);
				
		return 0;
	}
	
	public int txtSaveFile() {
		if (filename == null)
			return -1;
		try {
			FileWriter writer = new FileWriter(filename);
			editor.write(writer);
		} catch (IOException e) {
			System.err.println("Couldn't save file " + filename);
		}
		return 0;
	}
	
	public int txtSaveFileAs() {
		String newFilename = JavaGeneral.fileChooserSimpleMsg("Save");
		if (newFilename == null)
			return -1;
		filename = newFilename;
		this.setTitle("Edit - " + filename);
		
		return txtSaveFile();
	}
	
	public String txtGetText(long from, long to) {
		String text = null;
		int intFrom = new Long(from).intValue();
		int intTo = new Long(to).intValue();
		if (intFrom < 0)
			intFrom = 0;
		if (intFrom > editor.getDocument().getLength())
			intFrom = editor.getDocument().getLength();
		if (intTo < 0)
			intTo = 0;
		if (intTo > editor.getDocument().getLength())
			intTo = editor.getDocument().getLength();
		
		try {
			text = editor.getText(intFrom, intTo - intFrom);
		} catch (Exception e) {
			System.err.println("Invalid limits for getText");
		}
		return text;
	}
	
	public void txtAppend(String txt) {
		//editor.setText(editor.getText().concat(txt));
		
		int length = editor.getDocument().getLength();
		try {
			editor.getDocument().insertString(length, txt, null);
		} catch (Exception e) {
			System.err.println("txtAppend: Invalid position for text insertion");
		}
	}
	
	public String txtGetSelectedText() {
		String text = null;
		try {
			text = editor.getSelectedText();
		} catch (Exception e) {
			System.err.println("Selection doesn't have a valid mapping into the document for some reason");
		}
		return text;
	}
	
	public void txtReplace(long from, long to, String txt) {
		/*String oldText = editor.getText();
		String first = oldText.substring(0, new Long(from).intValue());
		String last = oldText.substring(new Long(to).intValue(), oldText.length());
		editor.setText(first + txt + last);
		*/
		
		try {
			editor.getDocument().remove(new Long(from).intValue(), new Long(to).intValue());
			editor.getDocument().insertString(new Long(from).intValue(), txt, null);
		} catch (Exception e) {
			System.err.println("txtReplace: Invalid limits for text replacement");
		}
	}
	
	public void txtSetSelection(long from, long to) {
		editor.setCaretPosition(new Long(from).intValue());
		editor.moveCaretPosition(new Long(to).intValue());
	}
	
	public void txtClose() {
		super.dispose();
	}
	
	/* Testing */
	public static void main(String args[]) throws Exception {
		String[][] menus = new String[][]{{"M&enu1","&Opcao1 do Menu1", "|",
			"Opcao2 do &Menu1"},{"Me&nu2","Opcao1 do M&enu2", "Op&cao2 do Menu2"}};
		JavaTextWindow w = new JavaTextWindow("ola", 100, 100, 500, 500, menus);
		w.txtOpenFile();
		/*System.out.println(w.txtGetText(10, 50));
		w.txtSetSelection(0, 5);
		System.out.println(w.txtGetSelectedText());
		
		w.txtAppend("azaza");
		*/
		
		//w.txtReplace(5, 9, "thing");
		
		//w.txtSaveFile();
	}
}
