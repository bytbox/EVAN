#include "app.hh"

#include "builtins.hh"

BlockSelector::BlockSelector() {
	setMovable(false);
	addAction(tr("hi"));
	addSeparator();
	addAction(tr("ho"));
}

