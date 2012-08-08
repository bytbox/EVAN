#include "app.hh"

int main(int argc, char *argv[]) {
	App app(argc, argv);
	MainWindow mw;
	mw.show();
	return app.exec();
}

