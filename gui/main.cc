#include "fltk.hh"

#define TITLE "EVAN"
#define WIDTH 680
#define HEIGHT 580
#define MENU_HEIGHT 28
#define TOOL_HEIGHT 30

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
	main_window = new Fl_Double_Window(WIDTH, HEIGHT, TITLE);
	menu_bar = new Fl_Menu_Bar(0, 0, WIDTH, MENU_HEIGHT);
	menu_bar->copy(menu_items);

	tool_bar = new Fl_Pack(0, MENU_HEIGHT+1, 80, 0);
	run_button = new Fl_Button(0, 0, 0, TOOL_HEIGHT, "Run");
	tool_bar->end();

	main_window->end();
}

void Cb_Quit(Fl_Widget *, void *) {
	delete main_window;
	exit(0);
}

int main(int argc, char *argv[]) {
	MkGui();
	main_window->show(argc, argv);
	return Fl::run();
}

