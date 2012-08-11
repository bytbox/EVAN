/*!
 * \file include/foreign.h
 * \brief Declarations of foreign functions
 *
 * This header file declares all known foreign functions, which can then be
 * accessed from both the interpreter and any statically linked code. In order
 * to increase portability, the exposed interfaces must be plain C (although
 * obviously, the backends will often involve other languages).
 *
 * The type of an exposed foreign function does not uniquely determine the type
 * of the corresponding EVAN block. The arguments to the function are the
 * concatenation of the parameters and arguments to the block - thus, the type
 * of a function exposed as taking a single parameter (Bool) versus that
 * exposed as taking a single argument (Bool) are indistinguishable to the
 * compiler. (For stylistic reasons, parameters are written with plain C types,
 * and arguments are written with the type definitions given below.)
 *
 * Because all types passed to and from the foreign interface must be declared
 * and handled manually (to keep the interface simple and compatible C), there
 * are some limitations. As noted above, only single values can be returned.
 * Multi-dimensional vectors are not supported, and lists (streams) are not
 * supported at all.
 *
 * \todo provide some proper error handling facility
 */

#ifndef FOREIGN_H
#define FOREIGN_H

#ifdef __cplusplus
extern "C" {
#endif

/*!
 * \brief For storing the size of a vector.
 */
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


Foreign LHCO_Input(const char *fname);

#ifdef __cplusplus
}
#endif

#endif /* !FOREIGN_H */

