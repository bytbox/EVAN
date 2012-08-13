#include "canvas.hh"

#include <QtGui>

#include <functional>

DialogFields *_commentDialogFields = NULL;
std::function<DialogFields *()> commentDialogFields =
([]() -> DialogFields *{
	if (_commentDialogFields) return _commentDialogFields;
 	_commentDialogFields = new DialogFields();
	return _commentDialogFields;
});

