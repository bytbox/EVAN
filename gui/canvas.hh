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

class BuiltinTool : public Tool {
	const Builtin &builtin;
public:
	explicit BuiltinTool(const Builtin &b);
	virtual ~BuiltinTool();
};

class CanvasComment : public QGraphicsItem {
public:
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
	CanvasScene();
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

