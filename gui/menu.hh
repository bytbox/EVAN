#ifndef MENU_HH
#define MENU_HH

#include <QtGui>

#include <string>
#include <vector>

class MenuAction : public QAction {
	Q_OBJECT;
public:
	MenuAction(const std::string &, QWidget *parent);
	MenuAction(const std::string &, QWidget *, const char *);
	MenuAction(const std::string &, QWidget *, const char *, const QKeySequence &);
};

class MenuItem {
public:
	enum Type {NIL, ACTION, SEPARATOR};

private:
	QAction *action;

	MenuItem(const Type, QAction *);

public:
	MenuItem();
	MenuItem(QAction *);
	operator QAction *() const;
	const Type type;
	
	static const MenuItem separator;
};

class Menu : public QMenu {
	Q_OBJECT;
public:
	Menu(const std::string &, const std::vector<MenuItem>);
};

class MenuBarManager {
	QMenuBar *menuBar;

public:
	MenuBarManager(QMenuBar *);

	MenuBarManager &addMenu(Menu *);
	MenuBarManager &addMenu(const std::string, const std::vector<MenuItem>);
};

#endif /* !MENU_HH */

