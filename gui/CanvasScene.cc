#include "canvas.hh"

CanvasScene::CanvasScene() {
	addText(tr("hi"));
	addItem(new CanvasComment("Hello, world!"));
}

