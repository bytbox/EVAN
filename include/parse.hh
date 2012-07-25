#ifndef PARSE_HH
#define PARSE_HH

#include "program.hh"

#include <string>

/**
 * \brief Parses a program read from the given filehandle.
 */
Program *parseProgram(FILE *);

/**
 * \brief Writes a program to the given file.
 */
void writeProgram(FILE *, const Program *);

#endif /* !PARSE_HH */

