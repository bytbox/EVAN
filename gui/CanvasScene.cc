#include "app.hh"
#include "canvas.hh"

CanvasScene::CanvasScene() : CanvasScene(
		new Program(new Block("Const", {0}, {}),
		       {new Comment("Hello, world!")})) {}

CanvasScene::CanvasScene(Program *program) : program(program) {
	for (Comment *c : program->comments)
		addItem(new CanvasComment(c));
}

Program *CanvasScene::getProgram() {
	// TODO extract block structure
	return program;
}

void CanvasScene::add(Comment *c) {
	qtLogger.debug("Adding comment: "+c->content);
	program->comments.push_back(c);
	addItem(new CanvasComment(c));
}

void CanvasScene::add(Block *b) {
	qtLogger.debug("Adding block");
	addItem(new CanvasBlock(b));
}

