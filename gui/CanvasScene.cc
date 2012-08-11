#include "canvas.hh"

CanvasScene::CanvasScene() {
	addItem(new CanvasComment("Hello, world!"));
}

CanvasScene::CanvasScene(Program *program) : program(program) {

}

Program *CanvasScene::getProgram() {

}

