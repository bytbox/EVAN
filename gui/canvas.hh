#ifndef CANVAS_HH
#define CANVAS_HH

#include "program.hh"
#include "builtins.hh"

#include <QtGui>

class Tool {
public:
	virtual void apply(Program *, const QPoint &, const QPoint &) const = 0;

	/*virtual void move(Program *, const QPoint &);
	virtual void down(Program *, const QPoint &);
	virtual void up(Program *, const QPoint &);*/
};

class DefaultTool : public Tool {
public:
	virtual void apply(Program *, const QPoint &, const QPoint &) const;
};

// Note that despite the name, this object is not associated with the Pipe
// class in program.hh.
class PipeTool : public Tool {
public:
};

class EachTool : public Tool {
public:
};

class BuiltinTool : public Tool {
	const Builtin &builtin;
public:
	explicit BuiltinTool(const Builtin &b);
	virtual void apply(Program *, const QPoint &, const QPoint &) const;
};

class CanvasPipe : public QGraphicsItem {
public:
};

class CanvasBlock : public QGraphicsItem {
public:
};

class CanvasEach : public QGraphicsItem {
public:
};

class CanvasScene : public QGraphicsScene {
	Q_OBJECT;

public:
};

class CanvasView : public QGraphicsView {
	Q_OBJECT;

	QPen pen;
	QBrush brush;

	const DefaultTool *defaultTool;
	const Tool *tool;
public:
	CanvasView();

	QSize minimumSizeHint() const;
	void paintEvent(QPaintEvent *);
};

#endif /* !CANVAS_HH */

