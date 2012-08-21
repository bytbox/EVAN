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

#ifndef NULL
#define NULL 0
#endif

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

/*!
 * \brief A "wildcard" foreign data type.
 *
 * The typechecking phase will know more about this, of course, but here all we
 * need to pass to and from the foreign interface is a pointer, to be cast
 * appropriately only by the functions which know what it is.
 */
typedef void *Foreign;

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

/*!
 * \brief A vector of foreign objects.
 *
 * This data structure can be used to hack around the single-dimensional
 * restriction on vectors.
 */
typedef struct {
	tLen len;
	Foreign *data;
} Vec_Foreign;

/*!
 * \brief A list of foreign objects.
 *
 * This structure can be thought of as a stream, or a lazy linked list, in
 * which items are not computed until they are needed.
 *
 * Lists of primitive, builtin types are not supported. Vectors of these types
 * are necessary for performance reasons, but since lists are pretty slow
 * anyways, the user might as well manually extract values as necessary.
 */
typedef struct {
	/*!
	 * \brief Delimits the state at the start of the stream. Must be passed
	 * to the iterator to retrieve the current item and the next state.
	 */
	void *state;

	/*!
	 * \brief The iterator function. The state passed to it is modified,
	 * and the resulting element (pointed to by the previous state) is
	 * returned.
	 *
	 * If the end of the list has been reached, the returned element will
	 * be NULL and the state will not be modified (meaning that asking for
	 * the next element on an empty stream is idempotent).
	 */
	Foreign (*iterator)(void *);
} List_Foreign;

/*!
 * \brief Advance the stream and return the old element.
 */
Foreign foreign_list_next(List_Foreign);

Vec_Foreign LHCO_Input(const char *);
Vec_Foreign LHCO_Parts(Foreign);
Foreign LHCO_Part_As_Track(Foreign);

Int Track_PDG_Id(Foreign);
Float Track_Energy(Foreign);
Float Track_Mass(Foreign);
Float Track_Px(Foreign);
Float Track_Py(Foreign);
Float Track_Pz(Foreign);

#ifdef __cplusplus
}
#endif

#endif /* !FOREIGN_H */

