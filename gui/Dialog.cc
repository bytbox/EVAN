#include "dialog.hh"

#include <QtGui>

Dialog::Dialog() : Dialog(NULL) {}
Dialog::Dialog(QWidget *parent) : QDialog(parent) {}
Dialog::~Dialog() {}

