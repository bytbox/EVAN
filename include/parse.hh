#ifndef PARSE_HH
#define PARSE_HH

#include "program.hh"

#include <string>

/**
 * \brief Parses a program read from the given filehandle.
 */
Program *parseProgram(FILE *);

#endif /* !PARSE_HH */

