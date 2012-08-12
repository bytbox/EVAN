#include "canvas.hh"

#include <QtGui>

DialogFields *commentDialogFields =
([]() -> DialogFields *{
 	DialogFields *fields = new DialogFields();
	return fields;
})();

