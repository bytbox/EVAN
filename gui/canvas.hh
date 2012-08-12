#ifndef CANVAS_HH
#define CANVAS_HH

#include "dialog.hh"
#include "builtins.hh"
#include "program.hh"

#include <QtGui>

#include <functional>

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
	const Comment *comment;
	QGraphicsRectItem *rect;
	QGraphicsTextItem *text;

	void updateText();
public:
	CanvasComment(const Comment *);
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

class Tool {
public:
	virtual ~Tool();

	virtual void apply(CanvasScene *, const QPoint &, std::function<void()>) const = 0;
};

class DefaultTool : public Tool {
public:
	virtual ~DefaultTool();

	virtual void apply(CanvasScene *, const QPoint &, std::function<void()>) const;
};

class CommentTool : public Tool {
public:
	virtual ~CommentTool();

	virtual void apply(CanvasScene *, const QPoint &, std::function<void()>) const;
};

// Note that despite the name, this object is not associated with the Pipe
// class in program.hh.
class PipeTool : public Tool {
public:
	virtual ~PipeTool();

	virtual void apply(CanvasScene *, const QPoint &, std::function<void()>) const;
};

class EachTool : public Tool {
public:
	virtual ~EachTool();

	virtual void apply(CanvasScene *, const QPoint &, std::function<void()>) const;
};

class ReturnTool : public Tool {
public:
	virtual ~ReturnTool();

	virtual void apply(CanvasScene *, const QPoint &, std::function<void()>) const;
};

class BuiltinTool : public Tool {
	const Builtin &builtin;
public:
	explicit BuiltinTool(const Builtin &b);
	virtual ~BuiltinTool();

	virtual void apply(CanvasScene *, const QPoint &, std::function<void()>) const;
};

/*!
 * \brief A specification of the fields to be displayed in a dialog.
 */
class DialogFields {
public:
};

class CreationDialog : public Dialog {
	Q_OBJECT;
public:
};

class EditDialog : public Dialog {
	Q_OBJECT;
public:
};

class CanvasView : public QGraphicsView {
	Q_OBJECT;

	QPen pen;
	QBrush brush;

	CanvasScene *canvasScene;
	const DefaultTool *defaultTool;
	const Tool *tool;

	// Allow lambda functions to access QGraphicsView::mousePressEvent.
	virtual void super_mousePressEvent(QMouseEvent *);
public:
	CanvasView();
	~CanvasView();

	void cancelTool();
	void setTool(const Tool *);

	QSize minimumSizeHint() const;

protected:
	virtual void mousePressEvent(QMouseEvent *);
};

#endif /* !CANVAS_HH */

