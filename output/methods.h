#ifndef METHODS_H
#define METHODS_H

#include <stdio.h>

void plain_number(FILE *, const char **, double);
void plain_bars(FILE *, const char **, int, double[]);
void plain_histogram(FILE *, const char **, int, double[]);
void plain_histogram2d(FILE *, const char **, int, int, double[]);

void ascii_number(FILE *, const char **, double);
void ascii_bars(FILE *, const char **, int, double[]);
void ascii_histogram(FILE *, const char **, int, double[]);
void ascii_histogram2d(FILE *, const char **, int, int, double[]);

void plot_number(FILE *, const char **, double);
void plot_bars(FILE *, const char **, int, double[]);
void plot_histogram(FILE *, const char **, int, double[]);
void plot_histogram2d(FILE *, const char **, int, int, double[]);

void root_number(FILE *, const char **, double);
void root_bars(FILE *, const char **, int, double[]);
void root_histogram(FILE *, const char **, int, double[]);
void root_histogram2d(FILE *, const char **, int, int, double[]);

#endif /* !METHODS_H */

