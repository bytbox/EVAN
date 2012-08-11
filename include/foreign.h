/*!
 * \file include/foreign.h
 * \brief Declarations of foreign functions
 *
 * This header file declares all known foreign functions, which can then be
 * accessed from both the interpreter and any statically linked code. In order
 * to increase portability, the exposed interfaces must be plain C (although
 * obviously, the backends will most likely involve several languages).
 *
 * Because all types passed to and from the foreign interface must be declared
 * and handled manually (to keep the interface simple and compatible C), there
 * are some limitations.
 *
 * \todo detail limitations of foreign interface.
 */

#ifndef FOREIGN_H
#define FOREIGN_H

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned short	tLen;

typedef char		Bool;
typedef long long	Int;
typedef double		Float;

typedef struct {
	tLen len;
	Bool *data;
} Vec_Bool;
typedef struct {
	tLen len;
	Int *data;
} Vec_Int;
typedef struct {
	tLen len;
	Float *data;
} Vec_Float;

typedef void *Foreign;

#ifdef __cplusplus
}
#endif

#endif /* !FOREIGN_H */

