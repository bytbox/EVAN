#ifndef HISTOGRAM_H
#define HISTOGRAM_H

/*!
 * \brief Histogram settings. This structure is passed, partially filled out,
 * to make_histogram(), and modified by said function with a complete
 * description of the contents of the returned double[] representing the
 * produced histogram.
 *
 * Values other than width and offset are ignored by make_histogram().
 */
struct histogram_opts {
	double width;
	double offset;

	double first;
	/*!
	 * \brief The starting point of the last bin. The last value
	 * potentially included in the histogram will be last+width.
	 */
	double last;
	unsigned int count;

	double *hist;
};

struct histogram_opts histogram(int, int);

void make_histogram(struct histogram_opts *, unsigned int, double[]);

#endif /* !HISTOGRAM_H */

