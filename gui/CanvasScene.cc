#include "canvas.hh"

CanvasScene::CanvasScene() {
	addItem(new CanvasComment("Hello, world!"));
}

