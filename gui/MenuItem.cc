#include "menu.hh"

MenuItem::MenuItem() : type(NIL) {}
MenuItem::MenuItem(const Type type, QAction *action) : action(action), type(type) {}
MenuItem::MenuItem(QAction *action) : action(action), type(ACTION) {}

const MenuItem MenuItem::separator = MenuItem(SEPARATOR, NULL);

MenuItem::operator QAction *() const {
	if (type != ACTION)
		throw "Attempted cast to (QAction *) of MenuItem with type != ACTION";
	return action;
}

