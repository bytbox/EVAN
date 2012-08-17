#include "app.hh"
#include "canvas.hh"

CanvasPipe::CanvasPipe(CanvasBlock *from, int retId, CanvasBlock *to, int argId)
: from(from), to(to), retId(retId), argId(argId)
{

}

CanvasPipe::~CanvasPipe() {

}

QRectF CanvasPipe::boundingRect() const {
	return from->boundingRect() | to->boundingRect();
}

void CanvasPipe::paint(QPainter *p, const QStyleOptionGraphicsItem *sogi, QWidget *w) {
	p->drawLine(	from->mapToScene(from->retPt(retId)),
			to->mapToScene(to->argPt(argId)));
}

