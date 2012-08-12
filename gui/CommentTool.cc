#include "app.hh"
#include "canvas.hh"
#include "program.hh"

CommentTool::~CommentTool() {

}

void CommentTool::apply(CanvasScene *s, const QPoint &p, std::function<void()> f) const {
	qtLogger.debug("Creating new comment");
	CreationDialog d(commentDialogFields);
}

