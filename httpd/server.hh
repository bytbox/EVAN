#ifndef SERVER_HH
#define SERVER_HH

#include <cstdio>
#include <string>

class Server {
	int sockfd, port;
public:
	/*!
	 * \brief Prepares a server on the specified port.
	 *
	 * The underlying socket will be created and bound, but nothing will be
	 * listening until run() is called.
	 */
	Server(int port);
	~Server();
	void run();

protected:
	/*!
	 * \brief Reads until a newline. The newline will not appear in the
	 * returned string - if CRLF is returned, neither byte will be present.
	 */
	char *readline();
	void write(const char *);
	void write(const std::string &);
};

#endif /* !SERVER_HH */

