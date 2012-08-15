#include "app.hh"
#include "canvas.hh"
#include "program.hh"

#include <QtGui>

CommentTool::~CommentTool() {

}

void CommentTool::apply(CanvasScene *s, const QPointF &p, std::function<void()> f) const {
	qtLogger.debug("Creating new comment");
	DialogFields *fields = commentDialogFields();
	CreationDialog d(fields);
	d.exec();
	if (d.result() != QDialog::DialogCode::Accepted) {
		qtLogger.debug("  (cancelled)");
		return;
	}

	Comment *comment = new Comment(fields->get("content"));
	comment->extraInfo.position[0] = p.x();
	comment->extraInfo.position[1] = p.y();
	s->add(comment);

	delete fields;
}

