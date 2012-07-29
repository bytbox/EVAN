#include "program.hh"

Comment::Comment(const std::string &content) : content(content) {

}

CommentExtra &Comment::extra() {
	return extraInfo;
}

