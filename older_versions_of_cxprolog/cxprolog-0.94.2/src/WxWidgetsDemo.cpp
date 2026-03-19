/*
 *   This file is part of the CxProlog system

 *   WXWidgetsDemo.cpp
 *   by A.Miguel Dias - 2006/01/08
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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

/* This file demonstrates how to write built-in predicates based on wxWidgets.
   There are three demo predicates defined in this file:
   
   w(+A,-W)
		Create a new window named A and returns in W a wxobject
		representing that window. There is a menu command in the window
		that enables the used to send predefined events to Prolog.
   t(+W,+A)
		Write in the existing window W the atom A.
   g(-T)
		Get the next available Prolog event (coming from Java or wxWidgets)

   The main lessons contained in this file are the following:
		- The implementation of each wxWidgets based built-in predicates
		  must be split between the CxProlog thread and the GUI thread.
		  This requires the definition of three different functions for
		  each built-in predicate.
		- The function WxDeleted(.) must be called in the destructor of
		  every wx object,  to warn CxProlog that the object does not exist
		  anymore.
		- High-level events are sent to Prolog using the function
          WxPostEvent(.). */

/* To activate this demo you define WXWIDGETS_DEMO as 1 */
#define WXWIDGETS_DEMO	1


#if USE_WXWIDGETS && WXWIDGETS_DEMO
#include <wx/wx.h>
#endif

extern "C" {
#include "CxProlog.h"
}

#if USE_WXWIDGETS && WXWIDGETS_DEMO

#if !wxUSE_GUI
#	error "GUI support required CxProlog with wxWidgets GUI."
#endif

/* Implements the window used in the demo */

class MyFrame: public wxFrame
{
public:
	MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size) ;
	~MyFrame() ;
	void WriteText(const wxString& text) { txtCtrl->WriteText(text) ; }
	void OnAbout(wxCommandEvent& event) ;
	void OnSendEvent(wxCommandEvent& event) ;
private:
	wxTextCtrl *txtCtrl ;
	DECLARE_EVENT_TABLE()
} ;

enum { ID_About = 1, ID_SendEvent } ;

BEGIN_EVENT_TABLE(MyFrame, wxFrame)
	EVT_MENU(ID_About,			MyFrame::OnAbout)
	EVT_MENU(ID_SendEvent,		MyFrame::OnSendEvent)
END_EVENT_TABLE()

MyFrame::MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size)
: wxFrame((wxFrame *)NULL, -1, title, pos, size)
{
	wxMenu *menuFile = new wxMenu;
	menuFile->Append( ID_About, "&About..." ) ;
	menuFile->Append( ID_SendEvent, "&Send Event" ) ;
	wxMenuBar *menuBar = new wxMenuBar;
	menuBar->Append( menuFile, "&File" ) ;
	SetMenuBar( menuBar ) ;
	CreateStatusBar() ;
	SetStatusText( "Welcome to wxWindows!" ) ;
	txtCtrl = new wxTextCtrl(this, -1, _T(""), wxPoint(0, 0), wxSize(0, 0),
							wxTE_MULTILINE | wxTE_READONLY) ;
}

MyFrame::~MyFrame()
{
	WxDeleted(this) ; /* IMPORTANT: warns cxprolog that the window is nomore */
}

void MyFrame::OnAbout(wxCommandEvent& WXUNUSED(event))
{
	wxMessageBox("This is a wxWindows CxProlog Test",
		"About CxProlog Test", wxOK | wxICON_INFORMATION, this) ;
}

void MyFrame::OnSendEvent(wxCommandEvent& WXUNUSED(event))
{
	static int n = 0 ;
	Str256 s ;
	WxPostEvent("so.si", "event", this, "number", n) ; /* IMPORTANT: post event */
	sprintf(s, "Sent event number %d.\n", n) ;
	WriteText(s) ;
	n++ ;
}

/* CXPROLOG C'BUILTINS */

/* Each wxWidgets-based built-in predicates must be written using three
   separated functions:
	- The first function runs in the CxProlog thread and performs the
	  analysis of the predicate arguments;
	- The second function runs in the GUI thread and implements the
	  required wxWidgets service;
	- The third function runs in the CxProlog thread and builds the results
	  of the predicate.
   The three functions must communicate through global variables.

   The name of the first function contains the suffix INIT, the name
   of the second function contains the suffix GUI, the name of the
   third function contains the suffix END. */


static CharPt s0 ;
static VoidPt o0 ;

/* Predicate to create a window */

static void PWCreateINIT() {	/* Process argument */
	s0 = XTestAtomName(X0) ;
}
static void PWCreateGUI() {		/* Perform the GUI service */
	MyFrame *w = new MyFrame(s0, wxPoint(50,50), wxSize(450,340)) ;
	w->Show(TRUE) ;
	o0 = w ;
}
void PWCreateEND() {		/* Build the results. */
	BindVarWithExtra(X1, MakeWxObj(o0)) ; /* Also handles ivars. */
}

/* Predicate to close window */

static void PWCloseINIT() {
	o0 = XTestWxObj(X0);
}
static void PWCloseGUI() {
	MyFrame *w = ((MyFrame *)o0) ;
	w->Close(TRUE) ;
}
static void PWCloseEND() {
	/* No results to build here */
}

/* Predicate to write in a window */

static void PWWriteINIT() { 
	o0 = XTestWxObj(X0) ; /* Get the wxobj addr and handles ivars. */
	s0 = XTestAtomName(X1) ;
}
static void PWWriteGUI() {
	MyFrame *w = ((MyFrame *)o0) ;
	w->WriteText(s0) ;
	w->WriteText("\n") ;
}
static void PWWriteEND() {
	/* No results to build here */
}

/* Predicate to create an alert */

static void PWAlertINIT() { 
	s0 = XTestAtomName(X0) ;
}
static void PWAlertGUI() {
	wxMessageDialog *d = new wxMessageDialog((wxFrame*)NULL, s0, "Message Box",
							 wxOK | wxICON_INFORMATION, wxDefaultPosition) ;
	d->ShowModal() ;
	d->Destroy() ;
}
static void PWAlertEND() {
	/* No results to build here */
}	


/* Predicate to get events comming from GUI */

static void PEventGet()
{
	MustBe( Unify(X0, GetEvent()) ) ;
}


void WxWidgetsDemoInit()
{
	InstallWxCBuiltinPred("wxw", 2, PWCreate) ;
	InstallWxCBuiltinPred("wxc", 1, PWClose) ;
	InstallWxCBuiltinPred("wxt", 2, PWWrite) ;
	InstallWxCBuiltinPred("wxa", 1, PWAlert) ;
	
	InstallCBuiltinPred("wxg", 1, PEventGet) ;
}

#else /* USE_WXWIDGETS && WXWIDGETS_DEMO */

void WxWidgetsDemoInit()
{
	/* nothing */
}

#endif /* USE_WXWIDGETS && WXWIDGETS_DEMO */
