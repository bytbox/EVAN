#include "canvas.hh"

CanvasBlock::CanvasBlock(Block *b) : block(b) {
	setFlag(ItemIsMovable, true);

	setPos(b->extraInfo.position.get(0), b->extraInfo.position.get(1));
	text = new QGraphicsTextItem(QString::fromStdString(b->fname));
	rect = new QGraphicsRectItem();
	ret = new QGraphicsRectItem();
	for (unsigned int i = 0; i < b->arguments.size(); i++) {
		auto ar = new QGraphicsRectItem();
		args.push_back(ar);
	}

	rect->setRect(text->boundingRect());
}

CanvasBlock::~CanvasBlock() {
	delete rect;
	delete text;
	delete ret;
	for (auto i : args)
		delete i;
}

QRectF CanvasBlock::boundingRect() const {
	return text->boundingRect();
}

void CanvasBlock::paint(QPainter *p, const QStyleOptionGraphicsItem *sogi, QWidget *w) {
	rect->paint(p, sogi, w);
	text->paint(p, sogi, w);
}

