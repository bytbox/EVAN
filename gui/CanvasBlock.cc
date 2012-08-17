#include "app.hh"
#include "canvas.hh"

CanvasBlock::CanvasBlock(Block *b) : block(b) {
	setFlag(ItemIsMovable, true);

	setPos(b->extraInfo.position.get(0), b->extraInfo.position.get(1));
	text = new QGraphicsTextItem(QString::fromStdString(b->fname));
	rect = new QGraphicsRectItem();
	ret = new QGraphicsRectItem();
	unsigned int argc = b->arguments.size();
	for (unsigned int i = 0; i < b->arguments.size(); i++) {
		auto ar = new QGraphicsRectItem();
		args.push_back(ar);
		argPipes.push_back(NULL);
	}

	rect->setRect(text->boundingRect());
	ret->setRect(rect->rect().left(), rect->rect().bottom(), rect->rect().width(), 14);
	double rw = rect->rect().width();
	double p = rect->rect().left();
	for (auto arg : args) {
		arg->setRect(p, rect->rect().top()-14, rw/argc, 14);
		p += rw/argc;
	}
}

CanvasBlock::~CanvasBlock() {
	delete rect;
	delete text;
	delete ret;
	for (auto i : args)
		delete i;
}

QRectF CanvasBlock::boundingRect() const {
	QRectF br = rect->boundingRect();
	if (args.size() > 0) br |= args[0]->boundingRect();
	br |= ret->boundingRect();
	return br;
}

void CanvasBlock::paint(QPainter *p, const QStyleOptionGraphicsItem *sogi, QWidget *w) {
	for (auto arg : args)
		arg->paint(p, sogi, w);
	rect->paint(p, sogi, w);
	ret->paint(p, sogi, w);
	text->paint(p, sogi, w);
}

void CanvasBlock::mousePressEvent(QGraphicsSceneMouseEvent *event) {
	if (event->pos().y() > boundingRect().height() - 14) {
		PipeTool *pt = new PipeTool(this);
		CanvasView::view->setTool(pt);
	}
}

void CanvasBlock::createPipe(CanvasScene *scene, CanvasBlock *from, int i, const QPointF &p) {
	if (p.y() > 14) {
		qtLogger.debug("No pipe created (argument not selected)");
		return;
	}
	qtLogger.debug("Creating pipe");
	int r = (int)(args.size() * (p.x()/rect->rect().width()));
	CanvasPipe *pipe = new CanvasPipe(from, i, this, r);
	scene->addItem(pipe);
}

QPointF CanvasBlock::retPt(int retId) const {
	return QPointF(ret->rect().width()/2, ret->rect().bottom());
}

QPointF CanvasBlock::argPt(int argId) const {
	double rw = rect->rect().width();
	int argc = args.size();
	double aw = rw/argc;
	return QPointF(aw/2 + aw*argId, args[0]->rect().top());
}

void CanvasBlock::mouseMoveEvent(QGraphicsSceneMouseEvent *event) {
	for (auto arg : argPipes)
		if (arg) arg->_prepareGeometryChange();
	QGraphicsItem::mouseMoveEvent(event);
	// update any associated pipes
	for (auto arg : argPipes)
		if (arg) arg->update(arg->boundingRect());
}

