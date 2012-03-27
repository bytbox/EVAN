#ifndef CANVAS_HH
#define CANVAS_HH

#include "program.hh"

#include "fltk.hh"

#include <FL/fl_draw.H>

#include <vector>

class WComment : public Fl_Widget {
public:
	WComment(Comment *);
	virtual void draw();
protected:
	Comment *c;
private:
};

class WBlock : public Fl_Widget {
public:
	virtual void draw();
protected:
private:
};

class Canvas : public Fl_Widget {
public:
	Canvas(int, int, int, int, Program *p = sample_program());
	virtual void draw();
protected:
	Program *p;
	std::vector<Fl_Widget *> parts;
private:
};

#endif /* !CANVAS_HH */

