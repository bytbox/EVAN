#ifndef APP_HH
#define APP_HH

#include "canvas.hh"
#include "dialog.hh"
#include "menu.hh"
#include "util.hh"

#include <QtGui>

#include <map>
#include <string>

class BlockSelector : public QToolBar {
	Q_OBJECT;

	CanvasView *canvasView;
public:
	BlockSelector(CanvasView *);
	~BlockSelector();

private:
	QSignalMapper *categoryMapper;
	QSignalMapper *builtinMapper;

	std::map <std::string, QMenu *> categoryMenu;
	std::map <std::string, BuiltinTool *> builtinTools;

private slots:
	void run();
	void cancel();
	void comment();
	void result();
	void each();

	void category(const QString &);
	void builtin(const QString &);
};

class MainPanel : public QWidget {
	Q_OBJECT;

	QVBoxLayout outsideLayout;
	QHBoxLayout mainLayout;

public:
	CanvasView canvas;

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

public:
	App(int argc, char *argv[]);
};

extern util::logging::logger qtLogger;

#endif /* !APP_HH */

