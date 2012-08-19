#include "foreign.h"
#include "tracks.h"

Int Track_PDG_Id(Foreign f) {
	return ((Track *)f)->pdgId;
}

Float Track_Energy(Foreign f) {
	return ((Track *)f)->energy;
}

Float Track_Mass(Foreign f) {
	return ((Track *)f)->mass;
}

Float Track_Px(Foreign f) {
	return ((Track *)f)->px;
}

Float Track_Py(Foreign f) {
	return ((Track *)f)->py;
}

Float Track_Pz(Foreign f) {
	return ((Track *)f)->pz;
}

