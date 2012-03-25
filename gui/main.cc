#include <iostream>
using namespace std;

#include "fltk.hh"

#define TITLE "EVAN"
#define MENU_HEIGHT 28
#define TOOL_HEIGHT 30
#define TB_WIDTH 100

/* Callbacks */
void Cb_Quit (Fl_Widget *, void *);

/* Singleton GUI Components */
Fl_Double_Window *main_window;
Fl_Menu_Bar *menu_bar;
Fl_Pack *tool_bar;
Fl_Button *run_button;

Fl_Menu_Item menu_items[] = {
	{ "&File",              0, 0, 0, FL_SUBMENU },
	{ "E&xit", FL_COMMAND + 'q', (Fl_Callback *)Cb_Quit, 0 },
	{ 0 },

	{ "&Help", 0, 0, 0, FL_SUBMENU },
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
	tool_bar->end();

	main_window->end();
}

void Cb_Quit(Fl_Widget *, void *) {
	delete main_window;
	exit(0);
}

int main(int argc, char *argv[]) {
	Fl::scheme("plastic");
	MkGui();
	main_window->show();
	return Fl::run();
}

