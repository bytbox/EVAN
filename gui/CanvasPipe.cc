#include "app.hh"
#include "canvas.hh"

CanvasPipe::CanvasPipe(CanvasBlock *from, int retId, CanvasBlock *to, int argId)
: from(from), to(to), retId(retId), argId(argId)
{

}

CanvasPipe::~CanvasPipe() {

}

QRectF CanvasPipe::boundingRect() const {
	qtLogger.debug("Bounding rect top: " + asString(from->mapToScene(from->boundingRect()).boundingRect().top()));
	return from->mapToScene(from->boundingRect()).boundingRect() | to->mapToScene(to->boundingRect()).boundingRect();
}

void CanvasPipe::paint(QPainter *p, const QStyleOptionGraphicsItem *sogi, QWidget *w) {
	qtLogger.debug("Painting canvas pipe");
	p->drawLine(	from->mapToScene(from->retPt(retId)),
			to->mapToScene(to->argPt(argId)));
}

void CanvasPipe::_prepareGeometryChange() {
	prepareGeometryChange();
}

