#ifndef METHODS_H
#define METHODS_H

#include <stdio.h>

void plain_number(FILE *, const char **, double);
void plain_bars(FILE *, const char **, int, double[]);
void plain_histogram(FILE *, const char **, int, double[]);
void plain_histogram2d(FILE *, const char **, int, int, double[]);

#endif /* !METHODS_H */

