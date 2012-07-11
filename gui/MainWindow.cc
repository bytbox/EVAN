#include "config.hh"

#include "app.hh"

MainWindow::MainWindow() : menus(menuBar()) {
	menus.addMenu("ho", std::vector<MenuItem>{new MenuAction("hi", this, this, SLOT(exit())), MenuItem::separator});

	mainLayout.addWidget(&blockSelector);
	mainLayout.addWidget(&canvas);
	outsideLayout.addLayout(&mainLayout);
	
	setWindowTitle(tr("EVAN"));
	statusBar()->showMessage(tr("EVAN")+" "+EVAN_VERSION);
}

void MainWindow::exit() {

}

