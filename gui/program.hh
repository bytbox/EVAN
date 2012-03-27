#ifndef PROGRAM_HH
#define PROGRAM_HH

class Position {
public:
	int x, y;
};

class Comment {
public:
	Position pos;
};

class Block {
public:
protected:
	Position pos;
private:
};

class Program {
public:
	char *filename;
protected:
private:
};

Program *sample_program();

#endif /* !PROGRAM_HH */

