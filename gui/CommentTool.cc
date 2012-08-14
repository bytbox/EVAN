#include "app.hh"
#include "canvas.hh"
#include "program.hh"

#include <QtGui>

CommentTool::~CommentTool() {

}

void CommentTool::apply(CanvasScene *s, const QPoint &p, std::function<void()> f) const {
	qtLogger.debug("Creating new comment");
	DialogFields *fields = commentDialogFields();
	CreationDialog d(fields);
	d.exec();
	if (d.result() != QDialog::DialogCode::Accepted) {
		qtLogger.debug("  (cancelled)");
		return;
	}

	Comment *comment = new Comment(fields->get("content"));
	s->add(comment);

	delete fields;
}

