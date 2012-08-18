#include "output.h"

#include <stdio.h>
#include <string.h>

const char *output_method_names[] = {"plain", "plot", "root", "ascii", 0};

const char *output_method_name(output_method_t meth) {
	return output_method_names[meth];
}

output_method_t output_method_from_name(const char *name) {
	const char **np = output_method_names;
	while (*np) {
		if (!strcasecmp(name, *np))
			return np - output_method_names;
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

