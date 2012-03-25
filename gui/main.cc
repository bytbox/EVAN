#include "fltk.hh"

#define WIDTH 680
#define HEIGHT 580
#define TITLE "EVAN"

/* Singleton GUI Components */
Fl_Double_Window *main_window;
Fl_Menu_Bar *menu_bar;
Fl_Menu_Button *file_menu, *help_menu;
Fl_Pack *toolbar;

void MkGui() {
	main_window = new Fl_Double_Window(WIDTH, HEIGHT, TITLE);

	/* Menus */
	menu_bar = new Fl_Menu_Bar(0, 0, 40, 40);

	

	main_window->end();
}

int main(int argc, char *argv[]) {
	MkGui();
	main_window->show(argc, argv);
	return Fl::run();
}
