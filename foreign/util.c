#include "foreign.h"

Foreign foreign_list_next(List_Foreign list) {
	Foreign n = list.iterator(list.state);
	return n;
}

