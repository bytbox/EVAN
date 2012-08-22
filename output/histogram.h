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
	/*!
	 * \brief The width of a single bin.
	 */
	double width;
	double offset;

	/*!
	 * \brief The starting point of the first bin. The first value
	 * potentially included in the histogram will be this value.
	 */
	double first;
	/*!
	 * \brief The starting point of the last bin. The last value
	 * potentially included in the histogram will be last+width.
	 */
	double last;
	unsigned int count;

	unsigned int *hist;
};

struct histogram_opts histogram(int, int);

void make_histogram(struct histogram_opts *, unsigned int, double[]);

#endif /* !HISTOGRAM_H */

