#include "program.hh"

Comment::Comment(const std::string &content) : content(content) {

}

Comment::~Comment() {}

CommentExtra &Comment::extra() {
	return extraInfo;
}

