#ifndef OUTPUT_H
#define OUTPUT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

typedef enum {PLAIN, PLOT, ROOT, ASCII} output_method_t;

extern const char *output_method_names[];

const char *output_method_name(output_method_t);
output_method_t output_method_from_name(const char *);

typedef struct {
	FILE *file;
	const char *basename;
	const char *filename;
} output_destination_t;

output_destination_t output_to_stdout();
output_destination_t output_to_file(FILE *);
output_destination_t output_to_basename(const char *);
output_destination_t output_to_filename(const char *);

void output_number(output_method_t, output_destination_t, const char **, double);
void output_bars(output_method_t, output_destination_t, const char **, int, double[]);
void output_histogram(output_method_t, output_destination_t, const char **, int, double[]);
void output_histogram2d(output_method_t, output_destination_t, const char **, int, int, double[]);

#ifdef __cplusplus
}
#endif

#endif /* !OUTPUT_H */

