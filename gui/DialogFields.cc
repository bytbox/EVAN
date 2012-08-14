#include "canvas.hh"

#include <functional>
#include <string>
using namespace std;

DialogFields::DialogFields() : DialogFields("") {}

DialogFields::DialogFields(const string &title) : title(title) {
	setLayout(&layout);
}

DialogFields::~DialogFields() {
	for (auto o : objects)
		delete o;
}

void DialogFields::addLineEdit(const string &label, const string &key) {
	addLineEdit(label, key, "");
}

void DialogFields::addLineEdit(const string &label, const string &key, const string &def) {
	QLineEdit *qle = new QLineEdit(QString::fromStdString(def), this);
	layout.addWidget(qle);
	objects.push_back(qle);
	properties[key] = ([qle]() -> string { return qle->text().toStdString(); });
}

void DialogFields::addTextEdit(const string &label, const string &key) {
	addTextEdit(label, key, "");
}

void DialogFields::addTextEdit(const string &label, const string &key, const string &def) {
	QTextEdit *qte = new QTextEdit(QString::fromStdString(def), this);
	qte->setAcceptRichText(false);
	layout.addWidget(qte);
	objects.push_back(qte);
	properties[key] = ([qte]() -> string { return qte->toPlainText().toStdString(); });
}

string DialogFields::get(const string &key) {
	auto i = properties.find(key);
	if (i != properties.end())
		return ((*i).second)();
	throw (new internal_error())->with(_POS);
}

