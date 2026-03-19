/*
 *   This file is part of the CxProlog system

 *   Gui.c
 *   by Henrique Oliveira & A.Miguel Dias - 2006/09/02
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
 
#include "CxProlog.h"

#if GUI_JAVA & GUI_WXWIDGETS
#	error "Cannot install two default GUIs at the same time."
#endif

#if GUI_JAVA

/* Predicate translation */
static CharPt jGeneralPredicatesStr="			\
gui_file_chooser_simple_msg(Msg, Fich) :-		\
	jgui_file_chooser_simple_msg(Msg, Fich).	\
gui_file_chooser_multiple_simple(List) :-		\
	jgui_file_chooser_multiple_simple(List).	\
gui_file_chooser_multiple_msg(Msg, List) :-		\
	jgui_file_chooser_multiple_msg(Msg, List).	\
gui_directory_chooser_msg(Msg, Dir) :-			\
	jgui_directory_chooser_msg(Msg, Dir).		\
gui_alert(Msg) :-								\
	jgui_alert(Msg).							\
gui_choice(Msg, Result) :-						\
	jgui_choice(Msg, Result).					\
gui_choice_list(List, Msg, Result) :-			\
	jgui_choice_list(List, Msg, Result).		\
gui_choice_yes_no_cancel(Msg, Result) :-		\
	jgui_choice_yes_no_cancel(Msg, Result).		\
gui_choice_ok_cancel(Msg, Result) :-			\
	jgui_choice_ok_cancel(Msg, Result).			\
gui_get_text(Msg, Result) :-					\
	jgui_get_text(Msg, Result).					\
" ;

static CharPt jGraphicPredicatesStr="										\
gui_gfx_create(Title, X0, Y0, Width, Height, Window) :-						\
	jgui_gfx_create(Title, X0, Y0, Width, Height, Window).					\
gui_gfx_putpixel(Window, X, Y, R, G, B, Pixel) :-							\
	jgui_gfx_putpixel(Window, X, Y, R, G, B, Pixel).						\
gui_gfx_line(Window, X0, Y0, X1, Y1, R, G, B, Line) :-						\
	jgui_gfx_line(Window, X0, Y0, X1, Y1, R, G, B, Line).					\
gui_gfx_circle(Window, X, Y, Radius, R, G, B, Circle) :-					\
	jgui_gfx_circle(Window, X, Y, Radius, R, G, B, Circle).					\
gui_gfx_rectangle(Window, X, Y, Width, Height, R, G, B, Rect) :-			\
	jgui_gfx_rectangle(Window, X, Y, Width, Height, R, G, B, Rect).			\
gui_gfx_circle_filled(Window, X, Y, Radius, R, G, B, Circle) :-				\
	jgui_gfx_circle_filled(Window, X, Y, Radius, R, G, B, Circle).			\
gui_gfx_rectangle_filled(Window, X, Y, Width, Height, R, G, B, Rect) :-		\
	jgui_gfx_rectangle_filled(Window, X, Y, Width, Height, R, G, B, Rect).	\
gui_gfx_draw_text(Window, Text, X, Y, T) :-									\
	jgui_gfx_draw_text(Window, Text, X, Y, T).								\
gui_gfx_button(Window, Text, X, Y, Button) :-								\
	jgui_gfx_button(Window, Text, X, Y, Button).							\
gui_gfx_list(Window, Items, X, Y, List) :-									\
	jgui_gfx_list(Window, Items, X, Y, List).								\
gui_gfx_list_getitem(List, Item) :-											\
	jgui_gfx_list_getitem(List, Item).										\
gui_gfx_delete_object(Window, Object) :-									\
	jgui_gfx_delete_object(Window, Object).									\
gui_gfx_clear(Window) :-													\
	jgui_gfx_clear(Window).													\
gui_gfx_close(Window) :-													\
	jgui_gfx_close(Window).													\
" ;

static CharPt jTextPredicatesStr="										\
gui_text_create(Title, X0, Y0, Width, Height, ListOfMenus, Window) :-		\
	jgui_text_create(Title, X0, Y0, Width, Height, ListOfMenus, Window).	\
gui_text_get_file_path(Window, FilePath) :-									\
	jgui_text_get_file_path(Window, FilePath).								\
gui_text_open_file(Window) :-												\
	jgui_text_open_file(Window).											\
gui_text_save_file(Window) :-												\
	jgui_text_save_file(Window).											\
gui_text_save_file_as(Window) :-											\
	jgui_text_save_file_as(Window).											\
gui_text_get_text(Window, From, To, Text) :-								\
	jgui_text_get_text(Window, From, To, Text).								\
gui_text_append(Window, Text) :-											\
	jgui_text_append(Window, Text).											\
gui_text_get_selected_text(Window, Text) :-									\
	jgui_text_get_selected_text(Window, Text).								\
gui_text_replace(Window, From, To, Text) :-									\
	jgui_text_replace(Window, From, To, Text).								\
gui_text_set_selection(Window, From, To) :-									\
	jgui_text_set_selection(Window, From, To).								\
gui_text_close(Window) :-													\
	jgui_text_close(Window).												\
" ;

void GuiInit()
{
	ZBasicLoadStr(jGeneralPredicatesStr) ;
	ZBasicLoadStr(jGraphicPredicatesStr) ;
	ZBasicLoadStr(jTextPredicatesStr) ;
}

#elif GUI_WXWIDGETS

static CharPt wxGeneralPredicatesStr = "		\
gui_file_chooser_simple_msg(Msg, Fich) :-		\
	wxgui_file_chooser_simple_msg(Msg, Fich).	\
gui_file_chooser_multiple_simple(List) :-		\
	wxgui_file_chooser_multiple_simple(List).	\
gui_file_chooser_multiple_msg(Msg, List) :-		\
	wxgui_file_chooser_multiple_msg(Msg, List).	\
gui_directory_chooser_msg(Msg, Dir) :-			\
	wxgui_directory_chooser_msg(Msg, Dir).		\
gui_alert(Msg) :-								\
	wxgui_alert(Msg).							\
gui_choice(Msg, Result) :-						\
	wxgui_choice(Msg, Result).					\
gui_choice_list(List, Msg, Result) :-			\
	wxgui_choice_list(List, Msg, Result).		\
gui_choice_yes_no_cancel(Msg, Result) :-		\
	wxgui_choice_yes_no_cancel(Msg, Result).	\
gui_choice_ok_cancel(Msg, Result) :-			\
	wxgui_choice_ok_cancel(Msg, Result).		\
gui_get_text(Msg, Result) :-					\
	wxgui_get_text(Msg, Result).				\
" ;

static CharPt wxGraphicPredicatesStr = "									\
gui_gfx_create(Title, X0, Y0, Width, Height, Window) :-						\
	wxgui_gfx_create(Title, X0, Y0, Width, Height, Window).					\
gui_gfx_putpixel(Window, X, Y, R, G, B, Pixel) :-							\
	wxgui_gfx_putpixel(Window, X, Y, R, G, B, Pixel).						\
gui_gfx_line(Window, X0, Y0, X1, Y1, R, G, B, Line) :-						\
	wxgui_gfx_line(Window, X0, Y0, X1, Y1, R, G, B, Line).					\
gui_gfx_circle(Window, X, Y, Radius, R, G, B, Circle) :-					\
	wxgui_gfx_circle(Window, X, Y, Radius, R, G, B, Circle).				\
gui_gfx_rectangle(Window, X, Y, Width, Height, R, G, B, Rectangle) :-		\
	wxgui_gfx_rectangle(Window, X, Y, Width, Height, R, G, B, Rectangle).	\
gui_gfx_circle_filled(Window, X, Y, Radius, R, G, B, Circle) :-				\
	wxgui_gfx_circle_filled(Window, X, Y, Radius, R, G, B, Circle).			\
gui_gfx_rectangle_filled(Window, X, Y, Width, Height, R, G, B, Rectangle) :- \
	wxgui_gfx_rectangle_filled(Window, X, Y, Width, Height, R, G, B, Rectangle). \
gui_gfx_draw_text(Window, Text, X, Y, T) :-									\
	wxgui_gfx_draw_text(Window, Text, X, Y, T).								\
gui_gfx_button(Window, Text, X, Y, Button) :-								\
	wxgui_gfx_button(Window, Text, X, Y, Button).							\
gui_gfx_list(Window, Items, X, Y, List) :-									\
	wxgui_gfx_list(Window, Items, X, Y, List).								\
gui_gfx_list_getitem(List, Item) :-											\
	wxgui_gfx_list_getitem(List, Item).										\
gui_gfx_delete_object(Window, Object) :-									\
	wxgui_gfx_delete_object(Window, Object).								\
gui_gfx_clear(Window) :-													\
	wxgui_gfx_clear(Window).												\
gui_gfx_close(Window) :-													\
	wxgui_gfx_close(Window).												\
" ;

static CharPt wxTextPredicatesStr = "										\
gui_text_create(Title, X0, Y0, Width, Height, ListOfMenus, Window) :-		\
	wxgui_text_create(Title, X0, Y0, Width, Height, ListOfMenus, Window).	\
gui_text_get_file_path(Window, FilePath) :-									\
	wxgui_text_get_file_path(Window, FilePath).								\
gui_text_open_file(Window) :-												\
	wxgui_text_open_file(Window).											\
gui_text_save_file(Window) :-												\
	wxgui_text_save_file(Window).											\
gui_text_save_file_as(Window) :-											\
	wxgui_text_save_file_as(Window).										\
gui_text_get_text(Window, From, To, Text) :-								\
	wxgui_text_get_text(Window, From, To, Text).							\
gui_text_append(Window, Text) :-											\
	wxgui_text_append(Window, Text).										\
gui_text_get_selected_text(Window, Text) :-									\
	wxgui_text_get_selected_text(Window, Text).								\
gui_text_replace(Window, From, To, Text) :-									\
	wxgui_text_replace(Window, From, To, Text).								\
gui_text_set_selection(Window, From, To) :-									\
	wxgui_text_set_selection(Window, From, To).								\
gui_text_close(Window) :-													\
	wxgui_text_close(Window).												\
" ;

void GuiInit()
{
	ZBasicLoadStr(wxGeneralPredicatesStr) ;
	ZBasicLoadStr(wxGraphicPredicatesStr) ;
	ZBasicLoadStr(wxTextPredicatesStr) ;
}

#else

void GuiInit()
{
		/* nothing */
}

#endif
