#include "config.hh"

#include "app.hh"
#include "dialog.hh"
#include "util.hh"

#include <QtGui>

#include <string>
using namespace std;

using Qt::ToolBarArea;

const QString MainWindow::fileFilters = tr("EVAN Source (*.evan)");

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
	qtLogger.debug("Selecting open file");
	string ofname = QFileDialog::getOpenFileName(
			this,					// parent
			tr("Open Program"),			// caption
			"",					// directory
			fileFilters				// filters
			).toStdString();
	if (ofname.size()) {
		// open ofname
	} else
		qtLogger.debug("No file selected");
}

void MainWindow::file_save() {
	if (!filename.isDefined()) {
		qtLogger.debug("Selecting save file");
		string sfname = QFileDialog::getSaveFileName(
				this,
				tr("Save Program"),
				"",
				fileFilters
				).toStdString();
		if (sfname.size()) filename = sfname;
		else {
			qtLogger.debug("No file selected");
			return;
		}
	}
	string fname = filename.get();
	qtLogger.debug("Saving '"+fname+"'");
	// save to fname
}

void MainWindow::file_save_as() {
	qtLogger.debug("Selecting save-as file");
	string fname = QFileDialog::getSaveFileName(
			this,
			tr("Save Program As"),
			"",
			fileFilters
			).toStdString();
	if (fname.size()) {
		// save to fname
	} else
		qtLogger.debug("No file selected");
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

