#include "histogram.h"

#include <math.h>
#include <stdlib.h>

struct histogram_opts histogram(int width, int offset) {
	struct histogram_opts opts = {width, offset};
	return opts;
}

void make_histogram(struct histogram_opts *opts, unsigned int dsz, double data[]) {
	if (dsz < 1) {
		// empty histogram
		opts->first = opts->last = opts->count = 0;
		opts->hist = 0;
		return;
	}
	
	// find the minimum and the maximum values
	double min, max;
	min = max = data[0];
	int i;
	for (i = 0; i < dsz; i++) {
		if (data[i] < min)
			min = data[i];
		if (data[i] > max)
			max = data[i];
	}

	// compute the rest of the histogram parameters
	{
		// min0 is `min` as if offset == 0
		double offset = opts->offset;
		double min0 = min - offset;
		double max0 = max - offset; // same deal
		opts->first = opts->width * floor(min0 / opts->width) + offset;
		opts->last = opts->width * ceil(max0 / opts->width) + offset;
		opts->count = ceil(max0 / opts->width) - floor(min0 / opts->width);
	}

	unsigned int *hist = (unsigned int *)calloc(opts->count, sizeof(unsigned int));
	for (i = 0; i < dsz; i++) {
		// find the bin in which data[i] belongs;
		double nd = data[i] - opts->first;
		int b = floor(nd / opts->width);
		hist[b]++;
	}
	opts->hist = hist;
}

