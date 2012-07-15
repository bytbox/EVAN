#include "menu.hh"

using namespace std;

Menu::Menu(const string &name, const vector<MenuItem> items) : QMenu(tr(name.data())) {
	for (auto item : items)
		switch (item.type) {
		case MenuItem::NIL:
			break; // ignore
		case MenuItem::ACTION:
			addAction(item);
			break;
		case MenuItem::SEPARATOR:
			addSeparator();
			break;
		}
}

