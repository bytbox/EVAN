#ifndef CANVAS_HH
#define CANVAS_HH

#include <QtGui>

class Tool {

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

