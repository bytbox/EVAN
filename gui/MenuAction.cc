#include "menu.hh"

MenuAction::MenuAction(const std::string &text, QWidget *parent)
	: QAction(tr(text.data()), parent) {}


// The actual QWidget API takes both a parent and a receiver, but since they're
// always (in practice) the same...
MenuAction::MenuAction(const std::string &text, QWidget *parentReceiver, const char *member) : MenuAction(text, parentReceiver) {
	connect(this, SIGNAL(triggered(bool)), parentReceiver, member);
}

MenuAction::MenuAction(const std::string &text, QWidget *parentReceiver, const char *member, const QKeySequence &ks) : MenuAction(text, parentReceiver, member) {
	setShortcut(ks);
}

