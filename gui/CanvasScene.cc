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
	// TODO extract updates
	return program;
}

void CanvasScene::add(Comment *c) {
	qtLogger.debug("Adding comment: "+c->content);
	// We don't update the program in parallel - that is saved for getProgram()
	addItem(new CanvasComment(c));
}

