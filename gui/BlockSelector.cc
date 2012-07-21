#include "app.hh"

#include "builtins.hh"

#include <string>
using namespace std;

BlockSelector::BlockSelector() {
	setMovable(false);

	categoryMapper = new QSignalMapper(this);
	builtinMapper = new QSignalMapper(this);
	connect(categoryMapper, SIGNAL(mapped(const QString &)), this, SLOT(category(const QString &)));

	addAction(tr("Run"), this, SLOT(run()));
	addSeparator();
	for (Category cat : BuiltinInfo::categories) {
		auto action = addAction(cat.name.data());
		connect(action, SIGNAL(triggered()), categoryMapper, SLOT(map()));
		categoryMapper->setMapping(action, cat.name.data());

		QMenu *m = new QMenu;
		for (Builtin b : cat.builtins) {
			m->addAction(b.name.data());
		}
		categoryMenu[cat.name] = m;
	}
}

void BlockSelector::run() {

}

void BlockSelector::category(const QString &qname) {
	string name = qname.toStdString();
	QMenu *menu = categoryMenu[name];
	auto pt = QCursor::pos();
	menu->popup(pt);
}

