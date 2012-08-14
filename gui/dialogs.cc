#include "canvas.hh"

#include <QtGui>

#include <functional>

std::function<DialogFields *()> commentDialogFields =
([]() -> DialogFields *{
 	DialogFields *fields = new DialogFields();
	fields->addTextEdit("", "content", "Hello, world!");
	return fields;
});

std::function<DialogFields *()> returnDialogFields =
([]() -> DialogFields *{
 	DialogFields *fields = new DialogFields();
	return fields;
});

