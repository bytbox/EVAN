#include "canvas.hh"

#include <functional>
#include <string>
using namespace std;

DialogFields::DialogFields() : DialogFields("") {}

DialogFields::DialogFields(const string &title) : title(title) {
	setLayout(&layout);
}

DialogFields::~DialogFields() {
	for (auto p : lineEdits)
		delete p.second;
	for (auto p : textEdits)
		delete p.second;
	for (auto l : labels)
		delete l;
}

void DialogFields::addLineEdit(const string &label, const string &key) {
	addLineEdit(label, key, "");
}

void DialogFields::addLineEdit(const string &label, const string &key, const string &def) {
	QLineEdit *qte = new QLineEdit(QString::fromStdString(def), this);
	layout.addWidget(qte);
	lineEdits[key] = qte;
}

void DialogFields::addTextEdit(const string &label, const string &key) {
	addTextEdit(label, key, "");
}

void DialogFields::addTextEdit(const string &label, const string &key, const string &def) {
	QTextEdit *qte = new QTextEdit(QString::fromStdString(def), this);
	layout.addWidget(qte);
	textEdits[key] = qte;
}

string DialogFields::get(const string &key) {
	return key;
}

