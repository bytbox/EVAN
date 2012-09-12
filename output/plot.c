// Plotting routines to interface with gnuplot

#include "histogram.h"
#include "output.h"
#include "methods.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

FILE *gnuplot();

void plot_number(FILE *f, const char **opts, double d) {
	// TODO
}

void plot_bars(FILE *f, const char **opts, int sz, double d[]) {
	// TODO
}

void plot_histogram(FILE *f, const char **opts, int sz, double d[]) {
	// Obviously, we can't pass our FILE object to gnuplot. We'll write to
	// a temporary file, and then read and copy that file to the given FILE
	// object.
	char *tmpfname = strdup(tmpnam(0));
	FILE *tmp = fopen(tmpfname, "w");
	// We don't have to construct the histogram ourselves - gnuplot can do
	// it for us. Just dump the data to the file.
	int i;
	for (i = 0; i < sz; i++)
		fprintf(tmp, "%g\n", d[i]);
	fclose(tmp);

	FILE *gp = gnuplot();
	// TODO
	fclose(gp);

	unlink(tmpfname);
	free(tmpfname);
}

void plot_contour(FILE *f, const char **opts, int xsz, int ysz, double d[]) {
	// TODO
}

FILE *gnuplot() {
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
	close(p[0]); // we don't actually need this
	return fdopen(p[1], "w");
}

