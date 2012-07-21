#ifndef APP_HH
#define APP_HH

#include <QtGui>

#include "canvas.hh"
#include "menu.hh"

class BlockSelector : public QToolBar {
	Q_OBJECT;
public:
	BlockSelector();

private:
	QSignalMapper *categoryMapper;
	QSignalMapper *builtinMapper;

private slots:
	void run();

	void category(const QString &);
};

class MainPanel : public QWidget {
	Q_OBJECT;

	QVBoxLayout outsideLayout;
	QHBoxLayout mainLayout;

	Canvas canvas;

public:
	MainPanel();
};

class MainWindow : public QMainWindow {
	Q_OBJECT;

	MenuBarManager menus;
	MainPanel mainPanel;
	
	BlockSelector blockSelector;

public:
	MainWindow();

private slots:
	void file_new();
	void file_open();
	void file_save();
	void file_save_as();
	void file_exit();

	void help_about();
};

class App : public QApplication {
	Q_OBJECT;

	MainWindow *mainWindow;
public:
	App(int argc, char *argv[]);
};

#endif /* !APP_HH */

