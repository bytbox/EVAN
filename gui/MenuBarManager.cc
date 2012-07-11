#include "menu.hh"

using namespace std;

MenuBarManager::MenuBarManager(QMenuBar *menuBar) : menuBar(menuBar) {

}

MenuBarManager &MenuBarManager::addMenu(Menu *menu) {
	menuBar->addMenu(menu);
	return *this;
}

MenuBarManager &MenuBarManager::addMenu(const string name, const vector<MenuItem> items) {
	addMenu( new Menu(name, items) );
	return *this;
}

