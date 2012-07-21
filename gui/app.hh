#ifndef APP_HH
#define APP_HH

#include "canvas.hh"
#include "menu.hh"

#include <QtGui>

#include <map>
#include <string>

class BlockSelector : public QToolBar {
	Q_OBJECT;
public:
	BlockSelector();

private:
	QSignalMapper *categoryMapper;
	QSignalMapper *builtinMapper;

	std::map <std::string, QMenu *> categoryMenu;

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
	void help_about_qt();
};

class App : public QApplication {
	Q_OBJECT;

	MainWindow *mainWindow;
public:
	App(int argc, char *argv[]);
};

#endif /* !APP_HH */

