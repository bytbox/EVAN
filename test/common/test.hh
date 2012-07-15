#ifndef TEST_HH
#define TEST_HH

#include <string>

#define TESTING

/*!
 * \brief An individual test case.
 */
class test {
	const std::string name;
public:
};

/*!
 * \brief A set of tests applying to a particular class or feature.
 */
class suite {
	const std::string name;
public:
	suite(const std::string &);
	void add(const test &);
};

/*!
 * \brief A set of suites applying to a particular module.
 */
class module {
public:
};

int main(int argc, char *argv[]);

#endif /* !TEST_HH */

