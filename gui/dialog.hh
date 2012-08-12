#ifndef DIALOG_HH
#define DIALOG_HH

#include <QtGui>

class Dialog : public QDialog {
	Q_OBJECT;
public:
	Dialog();
	Dialog(QWidget *);
	virtual ~Dialog();
};

#endif /* !DIALOG_HH */

