#include <iostream>

#include "canvas.hh"
#include "program.hh"

#include "fltk.hh"

#define TITLE "EVAN"
#define MENU_HEIGHT 28
#define TOOL_HEIGHT 30
#define TB_WIDTH 100

/* Callbacks */
void Cb_New (Fl_Widget *, void *);
void Cb_Open (Fl_Widget *, void *);
void Cb_Save (Fl_Widget *, void *);
void Cb_SaveAs (Fl_Widget *, void *);
void Cb_About (Fl_Widget *, void *);
void Cb_Quit (Fl_Widget *, void *);

/* GUI Actions */
void New();
void Open(const char *);

/* Singleton GUI Components */
Fl_Double_Window *main_window;
Fl_Menu_Bar *menu_bar;
Fl_Pack *tool_bar;
Canvas *canvas;
Fl_Button *run_button, *comment_button;

Fl_Window *about_window;

Fl_Menu_Item menu_items[] = {
	{ "&File",              0, 0, 0, FL_SUBMENU },
	{ "&New", FL_COMMAND + 'n', Cb_New, 0},
	{ "&Open", FL_COMMAND + 'o', Cb_Open, 0},
	{ "&Save", FL_COMMAND + 's', Cb_Save, 0},
	{ "Save &As", 0, Cb_SaveAs, 0},
	{ "E&xit", FL_COMMAND + 'q', Cb_Quit, 0 },
	{ 0 },

	{ "&Help", 0, 0, 0, FL_SUBMENU },
	{ "&About", 0, (Fl_Callback *)Cb_About, 0},
	{ 0 },


	{ 0 }
};

void MkGui() {
	int wh = Fl::h(), ww = Fl::w();
	main_window = new Fl_Double_Window(ww * .8, wh * .8, TITLE);
	menu_bar = new Fl_Menu_Bar(0, 0, main_window->w(), MENU_HEIGHT);
	menu_bar->copy(menu_items);

	tool_bar = new Fl_Pack(0, MENU_HEIGHT+1, TB_WIDTH, 0);
	run_button = new Fl_Button(0, 0, 0, TOOL_HEIGHT, "Run");
	comment_button = new Fl_Button(0, 0, 0, TOOL_HEIGHT, "Comment");
	tool_bar->end();

	canvas = new Canvas(TB_WIDTH+1, MENU_HEIGHT+1,
			main_window->w()-TB_WIDTH-2,
			main_window->h()-MENU_HEIGHT-2);

	main_window->end();

	about_window = new Fl_Window(ww * .2, wh * .2, "About EVAN");
	Fl_Text_Display *td = new Fl_Text_Display(0, 0, about_window->w(), about_window->h());
	Fl_Text_Buffer *ab = new Fl_Text_Buffer();
	ab->text("EVAN v0.1");
	td->buffer(ab);
	about_window->end();
}

void Cb_Quit(Fl_Widget *, void *) {
	delete main_window;
	exit(0);
}

void Cb_Save(Fl_Widget *, void *) {
	// TODO
}

void Cb_SaveAs(Fl_Widget *, void *) {
	// TODO
}

void Cb_Open(Fl_Widget *, void *) {
	Fl_File_Chooser *d = new Fl_File_Chooser(NULL, "EVAN Programs (*.evan)", Fl_File_Chooser::SINGLE, "TITLE");
	d->show();
	while (d->shown()) Fl::wait();
	const char *fn = d->value();
	if (fn != NULL) {
		Open(fn);
	}
	delete d;
}

void Open(const char *fn) {
	// TODO
}

void Cb_New(Fl_Widget *, void *) {
	New();
}

void New() {
	// TODO
}

void Cb_About(Fl_Widget *, void *) {
	about_window->show();
}

extern int yylex(void);

int main(int argc, char *argv[]) {
	Fl::scheme("plastic");
	MkGui();
	main_window->show();
	return Fl::run();
}

