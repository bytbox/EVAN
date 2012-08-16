#ifndef CANVAS_HH
#define CANVAS_HH

#include "dialog.hh"
#include "builtins.hh"
#include "program.hh"

#include <QtGui>

#include <functional>
#include <map>

class CanvasBlock;
class CanvasScene;

class CanvasItem : public QGraphicsItem {
public:
	virtual void createPipe(CanvasScene *, CanvasBlock *, const QPointF &);
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
	virtual ~CanvasComment();

	virtual QRectF boundingRect() const;
	virtual void paint(QPainter *, const QStyleOptionGraphicsItem *, QWidget *);
};

class CanvasPipe : public CanvasItem {
public:
	CanvasBlock *from, *to;

	CanvasPipe(CanvasBlock *, CanvasBlock *);
	virtual ~CanvasPipe();
	
	virtual QRectF boundingRect() const;
	virtual void paint(QPainter *, const QStyleOptionGraphicsItem *, QWidget *);
};

class CanvasBlock : public CanvasBlockItem {
	Block *block;

	QGraphicsRectItem *rect;
	QGraphicsTextItem *text;
	std::vector<QGraphicsRectItem *> args;
	QGraphicsRectItem *ret;

public:
	CanvasBlock(Block *);
	virtual ~CanvasBlock();

	virtual QRectF boundingRect() const;
	virtual void paint(QPainter *, const QStyleOptionGraphicsItem *, QWidget *);
	virtual void mousePressEvent(QGraphicsSceneMouseEvent *);
	virtual void createPipe(CanvasScene *, CanvasBlock *, const QPointF &);
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
	void add(Comment *);
	void add(Block *);
};

class Tool {
public:
	virtual ~Tool();

	virtual void apply(CanvasScene *, const QPointF &, std::function<void()>) const = 0;
};

class DefaultTool : public Tool {
public:
	virtual ~DefaultTool();

	virtual void apply(CanvasScene *, const QPointF &, std::function<void()>) const;
};

class CommentTool : public Tool {
public:
	virtual ~CommentTool();

	virtual void apply(CanvasScene *, const QPointF &, std::function<void()>) const;
};

// Note that despite the name, this object is not associated with the Pipe
// class in program.hh.
class PipeTool : public Tool {
	CanvasBlock *from;

public:
	PipeTool(CanvasBlock *);
	virtual ~PipeTool();

	virtual void apply(CanvasScene *, const QPointF &, std::function<void()>) const;
};

class EachTool : public Tool {
public:
	virtual ~EachTool();

	virtual void apply(CanvasScene *, const QPointF &, std::function<void()>) const;
};

class ReturnTool : public Tool {
public:
	virtual ~ReturnTool();

	virtual void apply(CanvasScene *, const QPointF &, std::function<void()>) const;
};

class BuiltinTool : public Tool {
	const Builtin builtin;
public:
	explicit BuiltinTool(const Builtin &b);
	virtual ~BuiltinTool();

	virtual void apply(CanvasScene *, const QPointF &, std::function<void()>) const;
};

/*!
 * \brief A specification of the fields to be displayed in a dialog.
 */
class DialogFields : public QWidget {
	Q_OBJECT;

	QVBoxLayout layout;

	std::map<std::string, std::function<std::string()>> properties;
	std::vector<QObject *> objects;
public:
	const std::string title;

	DialogFields();
	DialogFields(const std::string &);
	virtual ~DialogFields();

	void addLineEdit(const std::string &, const std::string &);
	void addLineEdit(const std::string &, const std::string &, const std::string &);

	void addTextEdit(const std::string &, const std::string &);
	void addTextEdit(const std::string &, const std::string &, const std::string &);

	virtual std::string get(const std::string &);
};

extern std::function<DialogFields *()> commentDialogFields, returnDialogFields;

class CreationDialog : public Dialog {
	Q_OBJECT;

	DialogFields *fields;

	QVBoxLayout layout;
	QHBoxLayout buttonLayout;

	QPushButton *okButton, *cancelButton;
public:
	CreationDialog(DialogFields *);
	virtual ~CreationDialog();
};

class EditDialog : public Dialog {
	Q_OBJECT;

	DialogFields *fields;

	QVBoxLayout layout;
	QHBoxLayout buttonLayout;

	QPushButton *okButton, *cancelButton;
public:
	EditDialog(DialogFields *);
	virtual ~EditDialog();
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
	static CanvasView *view;

	CanvasView();
	~CanvasView();

	void cancelTool();
	void setTool(const Tool *);

	QSize minimumSizeHint() const;

protected:
	virtual void mousePressEvent(QMouseEvent *);
};

#endif /* !CANVAS_HH */

