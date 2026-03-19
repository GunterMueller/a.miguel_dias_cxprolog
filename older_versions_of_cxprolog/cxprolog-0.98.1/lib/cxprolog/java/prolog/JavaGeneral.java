package prolog ;

/*
 *   This file is part of the CxProlog system

 *   JavaGeneral.java
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
import java.io.*;

public class JavaGeneral {
	
	private final static int CHOICE_ERROR = 0;
	private final static int CHOICE_OK = 1;
	private final static int CHOICE_CANCEL = 2;
	private final static int CHOICE_YES = 3;
	private final static int CHOICE_NO = 4;
	
    public static String fileChooserSimpleMsg(String msg) {    	
	    JFileChooser chooser = new JFileChooser();

	    // showDialog bloqueia antes de mostrar caixa de dialogo quando esta 
	    // funcao e' chamada no windows a partir do prolog com java_call
		int choice = chooser.showDialog(chooser, msg);
    
		if (choice == JFileChooser.APPROVE_OPTION)
			return chooser.getSelectedFile().getPath();
		else
			return null;
	}
	
	public static String[] fileChooserMultiple() {
		JFileChooser chooser = new JFileChooser();
		chooser.setMultiSelectionEnabled(true);
		
		int choice = chooser.showDialog(null, null);
		
		if (choice == JFileChooser.APPROVE_OPTION) {
			File[] selectedFiles = chooser.getSelectedFiles();
			String [] paths = new String[selectedFiles.length];
			for (int i = 0; i < selectedFiles.length; i++)
				paths[i] = selectedFiles[i].getPath();
			return paths;
		} 
		else
			return null; 
	}
	
	public static String[] fileChooserMultipleMsg(String msg) {
		JFileChooser chooser = new JFileChooser();
		chooser.setMultiSelectionEnabled(true);
		
		int choice = chooser.showDialog(null, msg);
		
		if (choice == JFileChooser.APPROVE_OPTION) {
			File[] selectedFiles = chooser.getSelectedFiles();
			String [] paths = new String[selectedFiles.length];
			for (int i = 0; i < selectedFiles.length; i++)
				paths[i] = selectedFiles[i].getPath();
			return paths;
		} 
		else
			return null; 
	}
	
	public static String fileChooserDirectoryMsg(String msg) {
		JFileChooser chooser = new JFileChooser();
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		
		int choice = chooser.showDialog(null, msg);
		
		if (choice == JFileChooser.APPROVE_OPTION)
			return chooser.getSelectedFile().getPath();
		else
			return null; 
	}
	
		
	public static void doAlert(String msg) throws Exception {
        JOptionPane.showMessageDialog(null, msg);
	}
	
	/* Used in predicates 
	 * gui_choice(+Msg, -Result), 
	 * gui_choice_list(+List, +Msg, -Result),
	 * gui_choice_yes_no_cancel(+Msg, -Result) and
	 * gui_choice_ok_cancel(+Msg, -Result).
	 * Returns the option selected by the user:
	 * 		0 --> Error
	 *		1 --> OK
	 *		2 --> CANCEL
	 *		3 --> YES
	 *		4 --> NO
	 */
	public static int doChoice(String msg, boolean yesno, 
			boolean ok, boolean cancel) {
		int choice = JOptionPane.CLOSED_OPTION; // default adequado?
		
		if (yesno && !ok && !cancel)
			choice = JOptionPane.showOptionDialog(null, msg, "Choice", 
			JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE, null, null, null);
		else if (yesno && !ok && cancel)
			choice = JOptionPane.showOptionDialog(null, msg, "Choice", 
				JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE, null, null, null);
		else if (!yesno && ok && cancel)
			choice = JOptionPane.showOptionDialog(null, msg, "Choice", 
				JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE, null, null, null);
		else {
			// Em caso de combinacao invalida de opcoes, mostrar
			// dialogo com ok - nao devera acontecer se forem usados os 
			// predicados gui_choice_yes_no_cancel(+Msg, -Result) e 
			// gui_choice_ok_cancel(+Msg, -Result).
			JOptionPane.showMessageDialog(null, msg);
			choice = JOptionPane.OK_OPTION;
		}
		
		switch (choice) {
			case JOptionPane.OK_OPTION:
				if (yesno && !ok)
					// casos yesno ou yesno+cancel
					return CHOICE_YES;	
				else 
					return CHOICE_OK;
			case JOptionPane.CANCEL_OPTION:
				return CHOICE_CANCEL;		
			//case JOptionPane.YES_OPTION:
			//	return CHOICE_YES;
			// OK_OPTION e YES_OPTION aparentam ter o mesmo valor
			case JOptionPane.NO_OPTION:
				return CHOICE_NO;
				
			case JOptionPane.CLOSED_OPTION:
				// que opcao se o utilizador fechar a janela?
				if (cancel)
					return CHOICE_CANCEL;
				else
					return CHOICE_NO;
				
			default:
				// Nunca devera ocorrer
				return CHOICE_ERROR;
		}
	}
	
	 
	public static String getText(String msg) {
		return JOptionPane.showInputDialog(null, msg);
	}
	
	public static void main(String args[]) throws Exception {
		//System.out.println("Selected file: " + fileChooserSimpleMsg("Choose file"));
		
		/*String[] paths = fileChooserMultiple();
		System.out.println("Selected files:");
		for (int i = 0; i < paths.length; i++) {
			System.out.println(i + ": " + paths[i]);
		}*/
		
		//System.out.println("Selected directory: " + fileChooserDirectoryMsg("Choose dir"));
		
		doAlert("ola");
		
		//System.out.println("Escolha: " + doChoice("Escolha",false,false,true));
		
		//System.out.println("Input: " + getText("Insira o texto"));
	}
}
