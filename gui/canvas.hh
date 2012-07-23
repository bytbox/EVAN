#ifndef CANVAS_HH
#define CANVAS_HH

#include "program.hh"

#include <QtGui>

class Tool {
public:
	void apply(Program *, const QPoint &, const QPoint &);
};

class DefaultTool : public Tool {
public:
};

class EachTool : public Tool {
public:
};

class BuiltinTool : public Tool {
public:
};

class Canvas : public QWidget {
	Q_OBJECT;

	QPen pen;
	QBrush brush;
public:
	Canvas();

	QSize minimumSizeHint() const;
	void paintEvent(QPaintEvent *);
};

#endif /* !CANVAS_HH */

