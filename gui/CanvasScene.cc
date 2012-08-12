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
	return program;
}

