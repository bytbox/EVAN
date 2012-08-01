#include "parsed.hh"

#include <string>
using namespace std;

ParsedComment::ParsedComment(const std::string *content) : content(content) {

}

ParsedComment::~ParsedComment() {
	delete content;
}

Comment *ParsedComment::extract(ParsedProgram *) {
	return new Comment(*content);
}

