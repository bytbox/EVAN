#ifndef TEST_HH
#define TEST_HH

#include <functional>
#include <map>
#include <string>
#include <vector>

#define TESTING

class test;
class suite;
class module;

/*!
 * \brief An individual test case.
 */
class test {
	friend suite;
	const std::string name;
	std::function<void ()> func;
	void run() const;
public:
	test(const std::string &, suite &, std::function<void ()>);
};

/*!
 * \brief A set of tests applying to a particular class or feature.
 */
class suite {
	const std::string name;
	std::vector<const test *> tests;
public:
	suite(const std::string &);
	suite(const std::string &, module &);
	void add(const test *);
	void run() const;
};

/*!
 * \brief A set of suites applying to a particular module.
 */
class module {
	std::string name;
	std::vector<const suite *> suites;
	static std::map<std::string, module> modules;
public:
	module();
	module(const std::string &);
	void add(const suite *);
	void run() const;

	static module &get(const std::string &);
	static void runAll();
};

#define SUITE(n, m) suite s(n, module::get(m))

int main(int argc, char *argv[]);

#endif /* !TEST_HH */

