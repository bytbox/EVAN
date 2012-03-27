#ifndef PROGRAM_HH
#define PROGRAM_HH

#include <string>
#include <vector>

struct Position {
	int x, y;
};

class Comment {
public:
	Comment(std::string text, Position pos) : text(text), pos(pos) {};
	std::string text;
	Position pos;
};

struct Block {
	Position pos;
};

struct Loop {
	Position ul;
	Position lr;
};

class Program {
public:
	const char *Name();

	static Program *sample();

	std::vector<Comment> comments;
	std::vector<Block> blocks;
	std::vector<Loop> loops;
protected:
	char *filename;
private:
};

Program *sample_program();

#endif /* !PROGRAM_HH */

