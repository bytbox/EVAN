#include "config.hh"

#include "app.hh"

MainWindow::MainWindow() : menus(menuBar()) {
	menus.addMenu("&File", {
			new MenuAction("&New", this, SLOT(exit())),
			new MenuAction("&Open", this, SLOT(exit())),
			new MenuAction("&Save", this, SLOT(exit())),
			new MenuAction("Save &As", this, SLOT(exit())),
			MenuItem::separator,
			new MenuAction("E&xit", this, SLOT(exit())),
			});

	menus.addMenu("&Help", {
			MenuItem::separator,
			new MenuAction("&About", this, SLOT(exit())),
			});

	mainLayout.addWidget(&blockSelector);
	mainLayout.addWidget(&canvas);
	outsideLayout.addLayout(&mainLayout);
	
	setWindowTitle(tr("EVAN"));
	statusBar()->showMessage(tr("EVAN")+" "+EVAN_VERSION);
}

void MainWindow::exit() {

}

