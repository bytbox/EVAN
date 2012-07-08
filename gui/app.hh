#ifndef APP_HH
#define APP_HH

#include <QtGui>

#include "canvas.hh"

class BlockSelector : public QWidget {
	Q_OBJECT;
public:
};

class App : public QApplication {
	Q_OBJECT;

	QVBoxLayout *outsideLayout;
	QHBoxLayout *mainLayout;

	BlockSelector *blockSelector;
public:
	App(int argc, char *argv[]);
};

#endif /* !APP_HH */

