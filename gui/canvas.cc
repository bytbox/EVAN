#include "canvas.hh"

#include <vector>
using namespace std;

WComment::WComment(Comment *c)
	: Fl_Widget(c->pos.x, c->pos.y, 5, 5), c(c) {

}

void WComment::draw() {
	draw_box(FL_BORDER_BOX, FL_CYAN);
}

Canvas::Canvas(int x, int y, int w, int h, Program *p) 
	: Fl_Widget(x, y, w, h, p->filename), p(p) {
	// TODO create widgets from program
}

void Canvas::draw() {
	draw_box(FL_DOWN_BOX, FL_WHITE);

	for (vector<Fl_Widget *>::iterator i = parts.begin(); i != parts.end(); i++) {
		(*i)->draw();
	}
}
