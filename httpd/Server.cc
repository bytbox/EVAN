/*!
 * \file httpd/Server.cc
 *
 * \todo httpd/Server really needs some sort of conditional compilation.
 */

#include "util.hh"
using util::system_error;

#include "server.hh"

#include <cstring>

// TODO need some sort of conditional compilation here
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

Server::Server(int port) : port(port) {
	sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd < 0)
		throw (new system_error());

	struct sockaddr_in addr;
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = INADDR_ANY;
	if (bind(sockfd, (struct sockaddr *)&addr, sizeof(addr)) < 0)
		throw (new system_error());
}

Server::~Server() {
	close(sockfd);
}

void Server::run() {
	// \TODO make size of queue configurable
	if (listen(sockfd, 5) < 0)
		throw new system_error;

	struct sockaddr_in addr;
	socklen_t alen = sizeof(addr);
	int fd = accept(sockfd, (struct sockaddr *)&addr, &alen);
	if (fd < 0)
		throw new system_error;
	// and handle it
}

char *Server::readline() {
	return NULL;
}

void write(const char *str) {

}

void write(const std::string &str) {
	write(str.c_str());
}

