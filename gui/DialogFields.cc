#include "canvas.hh"

#include <functional>
#include <string>
using namespace std;

DialogFields::DialogFields() : DialogFields("") {}

DialogFields::DialogFields(const string &title) : title(title) {

}

void DialogFields::addLineEdit(const string &label, const string &key) {
	addLineEdit(label, key, "");
}

void DialogFields::addLineEdit(const string &label, const string &key, const string &def) {

}

void DialogFields::addTextEdit(const string &label, const string &key) {
	addTextEdit(label, key, "");
}

void DialogFields::addTextEdit(const string &label, const string &key, const string &def) {

}

string DialogFields::get(const string &key) {
	return key;
}

