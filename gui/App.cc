#include "app.hh"

App::App(int argc, char *argv[]) : QApplication(argc, argv) {
	mainWindow = new MainWindow();

	mainWindow -> show();
}

