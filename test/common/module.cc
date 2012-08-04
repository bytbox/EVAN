#include "test.hh"

#include <iostream>
#include <map>
#include <string>
using namespace std;

module::module() : name() {}

module::module(const string &name) : name(name) {}

void module::add(const suite *s) {
	suites.push_back(s);
}

void module::run() const {
	for (const suite *s : suites)
		s->run();
}

map<string, module> module::modules;

module &module::get(const string &name) {
	map<string, module>::iterator i = modules.find(name);
	if (i == modules.end())
		modules[name] = module(name);
	return modules[name];
}

void module::runAll() {
	int i = 0;
	for (pair<string, module> p : modules) {
		i++;
		auto m = p.second;
		cerr << i << "/" << modules.size() << " " << m.name << flush;
		m.run();
		cerr << endl;
	}
}

