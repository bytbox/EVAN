#include "canvas.hh"

#include <functional>
#include <string>
using namespace std;

DialogFields::DialogFields() : DialogFields("") {}

DialogFields::DialogFields(const string &title) : title(title) {

}

string DialogFields::operator[](const string &key) {
	return key;
}

