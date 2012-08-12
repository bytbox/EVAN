#include "config.hh"

#include "app.hh"
#include "dialog.hh"

#include <QtGui>

#include <string>
using namespace std;

using Qt::ToolBarArea;

MainWindow::MainWindow() : menus(menuBar()), blockSelector(&mainPanel.canvas) {
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
	string ofname = QFileDialog::getOpenFileName(
			this,					// parent
			tr("Open Program"),			// caption
			"",					// directory
			tr("EVAN Source (*.evan)")		// filters
			).toStdString();
}

void MainWindow::file_save() {

}

void MainWindow::file_save_as() {

}

void MainWindow::file_exit() {
	this->close();
}

void MainWindow::help_about() {
	QMessageBox::about(this, tr("About EVAN"), "Some text here"); // TODO
}

void MainWindow::help_about_qt() {
	QMessageBox::aboutQt(this, tr("About Qt"));
}

