#ifndef TEST_HH
#define TEST_HH

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
	void run() const;
public:
	test(const std::string &, suite &);
};

/*!
 * \brief A set of tests applying to a particular class or feature.
 */
class suite {
	const std::string name;
	std::vector<const test *> tests;
public:
	suite(const std::string &);
	suite(const std::string &, module *);
	void add(const test *);
	void run() const;
};

/*!
 * \brief A set of suites applying to a particular module.
 */
class module {
	const std::string name;
	std::vector<const suite *> suites;
public:
	module(const std::string &);
	void add(const suite *);
	void run() const;
};

int main(int argc, char *argv[]);

#endif /* !TEST_HH */

