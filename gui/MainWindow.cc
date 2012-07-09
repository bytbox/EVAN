#include "config.hh"

#include "app.hh"

MainWindow::MainWindow() {
	mainLayout.addWidget(&blockSelector);
	mainLayout.addWidget(&canvas);
	outsideLayout.addLayout(&mainLayout);
	
	setWindowTitle(tr("EVAN"));
	statusBar()->showMessage(tr("EVAN")+" "+EVAN_VERSION);
}

