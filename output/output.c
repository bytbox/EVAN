#include "output.h"
#include "methods.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILENAME_MAX_LEN 50

const char *output_method_names[] = {"plain", "plot", "root", "ascii", 0};
const char *output_method_extensions[] = {"txt", "png", "root", "txt", 0};

const char *output_method_name(output_method_t meth) {
	return output_method_names[meth];
}

output_method_t output_method_from_name(const char *name) {
	const char **np = output_method_names;
	while (*np) {
		if (!strcasecmp(name, *np))
			return np - output_method_names;
		np++;
	}
	return -1;
}

output_destination_t output_to_stdout() {
	output_destination_t dest = {stdout, NULL, NULL};
	return dest;
}

output_destination_t output_to_file(FILE *f) {
	output_destination_t dest = {f, NULL, NULL};
	return dest;
}

output_destination_t output_to_basename(const char *bn) {
	output_destination_t dest = {NULL, bn, NULL};
	return dest;
}

output_destination_t output_to_filename(const char *fn) {
	output_destination_t dest = {NULL, NULL, fn};
	return dest;
}

void getFile(output_method_t meth, output_destination_t dest, char *mc, FILE **f) {
	char mustclose = 0;
	FILE *fout = dest.file;
	if (dest.filename) {
		fout = fopen(dest.filename, "r");
		mustclose = 1;
	}
	if (dest.basename) {
		char fname[FILENAME_MAX_LEN];
		snprintf(	fname,
				FILENAME_MAX_LEN,
				"%s.%s",
				dest.basename,
				output_method_extensions[meth]);
		mustclose = 1;
	}
	if (!fout) {
		perror("file not opened");
		exit(-1);
	}
	*f = fout;
	*mc = mustclose;
}

void output_number(output_method_t meth, output_destination_t dest, const char **opts, double d) {
	char mustclose;
	FILE *fout;
	getFile(meth, dest, &mustclose, &fout);
	switch (meth) {
	case PLAIN:
		plain_number(fout, opts, d);
		break;
	case ASCII:
	case ROOT:
	case PLOT:
		// TODO
		break;
	}
	if (mustclose)
		fclose(fout);
}

void output_bars(output_method_t meth, output_destination_t dest, const char **opts, int i, double d[]) {
	char mustclose;
	FILE *fout;
	getFile(meth, dest, &mustclose, &fout);
	switch (meth) {
	case PLAIN:
		plain_bars(fout, opts, i, d);
		break;
	case ASCII:
	case ROOT:
	case PLOT:
		// TODO
		break;
	default:
		assert(0);
	}
	if (mustclose)
		fclose(fout);
}

void output_histogram(output_method_t meth, output_destination_t dest, const char **opts, int i, double d[]) {
	char mustclose;
	FILE *fout;
	getFile(meth, dest, &mustclose, &fout);
	switch (meth) {
	case PLAIN:
		plain_histogram(fout, opts, i, d);
		break;
	case ASCII:
	case ROOT:
	case PLOT:
		// TODO
		break;
	}
	if (mustclose)
		fclose(fout);
}

void output_histogram2d(output_method_t meth, output_destination_t dest, const char **opts, int xsz, int ysz, double d[]) {
	char mustclose;
	FILE *fout;
	getFile(meth, dest, &mustclose, &fout);
	switch (meth) {
	case PLAIN:
		plain_histogram2d(fout, opts, xsz, ysz, d);
		break;
	case ASCII:
	case ROOT:
	case PLOT:
		// TODO
		break;
	}
	if (mustclose)
		fclose(fout);
}

