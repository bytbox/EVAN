#include "program.hh"

#include <string>
using namespace std;

Pipe::~Pipe() {
	if (comment) delete comment;
}

string Pipe::getComment() {
	if (comment)
		return comment->content;
	return "";
}

bool Pipe::hasComment() {
	if (comment) return true;
	return false;
}

void Pipe::setComment(const string &newcomment) {
	if (comment) delete comment;
	comment = new Comment(newcomment);
}

