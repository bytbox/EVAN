#include "menu.hh"

MenuAction::MenuAction(const std::string &text, QWidget *parent)
	: QAction(tr(text.data()), parent) {}

MenuAction::MenuAction(const std::string &text, QWidget *parent, const QObject *receiver, const char *member) : MenuAction(text, parent) {
	connect(this, SIGNAL(triggered(bool)), receiver, member);
}

MenuAction::MenuAction(const std::string &text, QWidget *parent, const QObject *receiver, const char *member, const QKeySequence &ks) : MenuAction(text, parent, receiver, member) {
	setShortcut(ks);
}

