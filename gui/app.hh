#ifndef APP_HH
#define APP_HH

#include <QtGui>

#include "canvas.hh"
#include "menu.hh"

class BlockSelector : public QWidget {
	Q_OBJECT;
public:
};

class MainWindow : public QMainWindow {
	Q_OBJECT;

	QVBoxLayout outsideLayout;
	QHBoxLayout mainLayout;

	BlockSelector blockSelector;
	Canvas canvas;

	MenuBarManager menus;
public:
	MainWindow();

private slots:
	void exit();
};

class App : public QApplication {
	Q_OBJECT;

	MainWindow *mainWindow;
public:
	App(int argc, char *argv[]);
};

#endif /* !APP_HH */

