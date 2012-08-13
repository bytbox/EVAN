#include "app.hh"
#include "canvas.hh"
#include "program.hh"

CommentTool::~CommentTool() {

}

void CommentTool::apply(CanvasScene *s, const QPoint &p, std::function<void()> f) const {
	qtLogger.debug("Creating new comment");
	auto fields = commentDialogFields();
	CreationDialog d(fields);
	d.exec();

	Comment *comment = new Comment("some text");
	s->add(comment);
}

