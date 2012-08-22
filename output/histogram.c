#include "histogram.h"

#include <stdlib.h>

struct histogram_opts histogram(int width, int offset) {
	struct histogram_opts opts = {width, offset};
	return opts;
}

void make_histogram(struct histogram_opts *opts, unsigned int dsz, double data[]) {
	if (dsz < 1) {
		// empty histogram
		return;
	}
	double *hist = (double *)malloc(sizeof(double));
	opts->hist = hist;
}

