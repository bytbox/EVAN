#include "canvas.hh"

CanvasPipe::CanvasPipe(CanvasBlock *from, CanvasBlock *to) : from(from), to(to) {

}

CanvasPipe::~CanvasPipe() {

}

QRectF CanvasPipe::boundingRect() const {
	return from->boundingRect() | to->boundingRect();
}

void CanvasPipe::paint(QPainter *p, const QStyleOptionGraphicsItem *sogi, QWidget *w) {

}

