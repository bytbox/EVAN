// Plotting routines to interface with gnuplot

#include "histogram.h"
#include "output.h"
#include "methods.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

void plot_number(FILE *f, const char **opts, double d) {
	// TODO
}

void plot_bars(FILE *f, const char **opts, int sz, double d[]) {
	// TODO
}

void plot_histogram(FILE *f, const char **opts, int sz, double d[]) {
	struct histogram_opts hist = histogram(1, 0);
	make_histogram(&hist, sz, d);
	int i;
	double c = hist.first;
	for (i = 0; i < hist.count; i++) {
		fprintf(f, "%f %d\n", c, hist.hist[i]);
		c += hist.width;
	}
}

void plot_contour(FILE *f, const char **opts, int xsz, int ysz, double d[]) {
	// TODO
}

void start_gnuplot() {
	int p[2]; // read from p[0], write to p[1]
	if (pipe(p)) {
		perror("could not create pipes for gnuplot");
		exit(1);
	}
	pid_t pid = fork();
	if (pid < 0) {
		perror("could not fork");
		exit(1);
	}

	if (!pid) {
		// child
		close(p[1]);
		dup2(p[0], 0);
		char *args[] = {0};
		if (execvp("gnuplot", args) < 0) {
			perror("error starting gnuplot");
			exit(1);
		}
	}

	// parent
	close(p[0]);
	int gnuplot = p[1];
}

