/*!
 * \file include/foreign.h
 * \brief Declarations of foreign functions
 *
 * This header file declares all known foreign functions, which can then be
 * accessed from both the interpreter and any statically linked code. In order
 * to increase portability, the exposed interfaces must be plain C (although
 * obviously, the backends will most likely involve several languages).
 */

#ifndef FOREIGN_H
#define FOREIGN_H

#ifdef __cplusplus
extern "C" {
#endif

typedef signed long long Int;
typedef double Float;

#ifdef __cplusplus
}
#endif

#endif /* !FOREIGN_H */

