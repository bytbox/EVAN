#include "output.h"

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

