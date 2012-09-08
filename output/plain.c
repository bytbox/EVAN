#include "histogram.h"
#include "output.h"
#include "methods.h"

#include <stdio.h>

void plain_number(FILE *f, const char **opts, double d) {
	fprintf(f, "%g\n", d);
}

void plain_bars(FILE *f, const char **opts, int sz, double d[]) {
	int i;
	for (i=0; i<sz; i++)
		fprintf(f, "%g\n", d[i]);
}

void plain_histogram(FILE *f, const char **opts, int sz, double d[]) {
	struct histogram_opts hist = histogram(1, 0);
	make_histogram(&hist, sz, d);
	int i;
	double c = hist.first;
	for (i = 0; i < hist.count; i++) {
		fprintf(f, "%g: %d\n", c, hist.hist[i]);
		c += hist.width;
	}
}

void plain_contour(FILE *f, const char **opts, int xsz, int ysz, double d[]) {
	// TODO
}

