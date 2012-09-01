#include "histogram.h"
#include "output.h"
#include "methods.h"

#include <stdio.h>

void ascii_number(FILE *f, const char **opts, double d) {
	fprintf(f, "%f\n", d);
}

void ascii_bars(FILE *f, const char **opts, int sz, double d[]) {
	int i;
	for (i=0; i<sz; i++)
		fprintf(f, "%f\n", d[i]);
}

void ascii_histogram(FILE *f, const char **opts, int sz, double d[]) {
	struct histogram_opts hist = histogram(1, 0);
	make_histogram(&hist, sz, d);
	int i;
	double c = hist.first;
	for (i = 0; i < hist.count; i++) {
		fprintf(f, "%f: %d\n", c, hist.hist[i]);
		c += hist.width;
	}
}

void ascii_contour(FILE *f, const char **opts, int xsz, int ysz, double d[]) {
	// TODO
}

