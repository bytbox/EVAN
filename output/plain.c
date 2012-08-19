#include "output.h"
#include "methods.h"

#include <stdio.h>

void plain_number(FILE *f, const char **opts, double d) {
	fprintf(f, "%f\n", d);
}

void plain_bars(FILE *f, const char **opts, int sz, double d[]) {
	int i;
	for (i=0; i<sz; i++)
		fprintf(f, "%f\n", d[i]);
}

void plain_histogram(FILE *f, const char **opts, int sz, double d[]) {
	// TODO
}

void plain_histogram2d(FILE *f, const char **opts, int xsz, int ysz, double d[]) {
	// TODO
}

