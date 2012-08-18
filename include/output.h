#ifndef OUTPUT_H
#define OUTPUT_H

typedef enum {PLAIN, PLOT, ROOT} output_method_t;

extern const char *output_method_names[];

const char *output_method_name(output_method_t);
output_method_t output_method_from_name(const char *);

#endif /* !OUTPUT_H */

