#include "app.hh"

#include "builtins.hh"

#include <iostream>

BlockSelector::BlockSelector() {
	setMovable(false);

	addAction(tr("Compile"));
	addAction(tr("Run"));
	addSeparator();
	for (Category cat : BuiltinInfo::categories) {
		addAction(cat.name.data());
	}
}

