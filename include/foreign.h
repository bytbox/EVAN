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

typedef struct List_Bool_st {
	Bool head;
	struct List_Bool_st *tail;
} List_Bool;
typedef struct List_Int_st {
	Int head;
	struct List_Int_st *tail;
} List_Int;
typedef struct List_Float_st {
	Float head;
	struct List_Float_st *tail;
} List_Float;

typedef void *Foreign;

#ifdef __cplusplus
}
#endif

#endif /* !FOREIGN_H */

