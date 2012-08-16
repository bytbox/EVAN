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
	}

	rect->setRect(text->boundingRect());
	ret->setRect(rect->rect().left(), rect->rect().bottom(), rect->rect().width(), 14);
	double rw = rect->rect().width();
	double p = rect->rect().left();
	for (auto arg : args) {
		arg->setRect(p, rect->rect().top()-10, rw/argc, 10);
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
	if (event->pos().y() > boundingRect().height() - 10) {
		PipeTool *pt = new PipeTool(this);
		CanvasView::view->setTool(pt);
	}
}

void CanvasBlock::createPipe(CanvasBlock *from, const QPointF &p) {
}

