#ifndef CANVAS_HH
#define CANVAS_HH

#include "program.hh"
#include "builtins.hh"

#include <QtGui>

class Tool {
public:
	virtual ~Tool();

	/*virtual void move(Program *, const QPoint &);
	virtual void down(Program *, const QPoint &);
	virtual void up(Program *, const QPoint &);*/
};

class DefaultTool : public Tool {
public:
	virtual ~DefaultTool();
};

class CommentTool : public Tool {
public:
	virtual ~CommentTool();
};

// Note that despite the name, this object is not associated with the Pipe
// class in program.hh.
class PipeTool : public Tool {
public:
	virtual ~PipeTool();
};

class EachTool : public Tool {
public:
	virtual ~EachTool();
};

class ReturnTool : public Tool {
public:
	virtual ~ReturnTool();
};

class BuiltinTool : public Tool {
	const Builtin &builtin;
public:
	explicit BuiltinTool(const Builtin &b);
	virtual ~BuiltinTool();
};

class CanvasItem : public QGraphicsItem {
public:
protected:
};

class CanvasBlockItem : public CanvasItem {
public:
protected:
	virtual void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *);
};

class CanvasContainerItem : public CanvasItem {
public:
protected:
};

class CanvasComment : public CanvasBlockItem {
	std::string content;
	QGraphicsRectItem *rect;
	QGraphicsTextItem *text;

	void updateText();
public:
	CanvasComment(const std::string &);
	~CanvasComment();

	virtual QRectF boundingRect() const;
	virtual void paint(QPainter *, const QStyleOptionGraphicsItem *, QWidget *);
};

class CanvasPipe : public CanvasItem {
public:
};

class CanvasBlock : public CanvasBlockItem {
public:
};

class CanvasReturn : public CanvasBlock {
public:
};

class CanvasEach : public CanvasContainerItem {
public:
};

class CanvasScene : public QGraphicsScene {
	Q_OBJECT;

	Program *program;

public:
	CanvasScene();
	CanvasScene(Program *);

	Program *getProgram();
};

class CanvasView : public QGraphicsView {
	Q_OBJECT;

	QPen pen;
	QBrush brush;

	const DefaultTool *defaultTool;
	const Tool *tool;
public:
	CanvasView();
	~CanvasView();

	QSize minimumSizeHint() const;
};

#endif /* !CANVAS_HH */

