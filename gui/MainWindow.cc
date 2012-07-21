#include "config.hh"

#include "app.hh"

using Qt::ToolBarArea;

MainWindow::MainWindow() : menus(menuBar()) {
	menus.addMenu("&File", {
			new MenuAction("&New", this, SLOT(file_new())),
			new MenuAction("&Open", this, SLOT(file_open())),
			new MenuAction("&Save", this, SLOT(file_save())),
			new MenuAction("Save &As", this, SLOT(file_save_as())),
			MenuItem::separator,
			new MenuAction("E&xit", this, SLOT(file_exit())),
			});

	menus.addMenu("&Help", {
			MenuItem::separator,
			new MenuAction("&About", this, SLOT(help_about())),
			new MenuAction("About &Qt", this, SLOT(help_about_qt())),
			});

	addToolBar(ToolBarArea::LeftToolBarArea, &blockSelector);

	setCentralWidget(&mainPanel);
	
	setWindowTitle(tr("EVAN"));
	statusBar()->showMessage(tr("EVAN")+" "+EVAN_VERSION);
}

void MainWindow::file_new() {

}

void MainWindow::file_open() {

}

void MainWindow::file_save() {

}

void MainWindow::file_save_as() {

}

void MainWindow::file_exit() {
	this->close();
}

void MainWindow::help_about() {
	// TODO
	//QMessageBox::about(this, "Hello", "Some <b>text</b>");
}

void MainWindow::help_about_qt() {
	// TODO
	//QMessageBox::aboutQt(this, tr("huh?"));
}

