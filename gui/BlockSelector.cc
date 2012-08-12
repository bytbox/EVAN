#include "app.hh"

#include "builtins.hh"

#include <iostream>
#include <string>
using namespace std;

BlockSelector::BlockSelector(CanvasView *cView) : canvasView(cView) {
	setMovable(false);

	categoryMapper = new QSignalMapper(this);
	builtinMapper = new QSignalMapper(this);
	connect(categoryMapper, SIGNAL(mapped(const QString &)), this, SLOT(category(const QString &)));
	connect(builtinMapper, SIGNAL(mapped(const QString &)), this, SLOT(builtin(const QString &)));

	addAction(tr("Run"), this, SLOT(run()));
	addSeparator();
	addAction(tr("Cancel"), this, SLOT(cancel()));
	addAction(tr("Comment"), this, SLOT(comment()));
	addAction(tr("Return"), this, SLOT(result()));
	addAction(tr("Each Loop"), this, SLOT(each()));
	for (Category cat : BuiltinInfo::categories) {
		auto action = addAction(cat.name.data());
		connect(action, SIGNAL(triggered()), categoryMapper, SLOT(map()));
		categoryMapper->setMapping(action, cat.name.data());

		QMenu *m = new QMenu;
		for (Builtin b : cat.builtins) {
			auto ba = m->addAction(b.name.data());
			connect(ba, SIGNAL(triggered()), builtinMapper, SLOT(map()));
			builtinMapper->setMapping(ba, b.name.data());
			builtinTools[b.name] = new BuiltinTool(b);
		}
		categoryMenu[cat.name] = m;
	}
}

BlockSelector::~BlockSelector() {
	delete categoryMapper;
	delete builtinMapper;
	for (pair<string, QMenu *> p : categoryMenu)
		delete p.second;
}

void BlockSelector::run() {

}

void BlockSelector::cancel() {
	canvasView->cancelTool();
}

void BlockSelector::comment() {
	qtLogger.debug("Selecting comment tool");
	canvasView->setTool(new CommentTool());
}

void BlockSelector::result() {
	qtLogger.debug("Selecting result tool");
	canvasView->setTool(new ReturnTool());
}

void BlockSelector::each() {
	qtLogger.debug("Selecting each tool");
	canvasView->setTool(new EachTool());
}

void BlockSelector::category(const QString &qname) {
	string name = qname.toStdString();
	QMenu *menu = categoryMenu[name];
	auto pt = QCursor::pos();
	menu->popup(pt);
}

void BlockSelector::builtin(const QString &qname) {
	string name = qname.toStdString();
	qtLogger.debug("Selecting builtin tool: " + name);
}

