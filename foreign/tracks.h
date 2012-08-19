#ifndef TRACKS_H
#define TRACKS_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int pdgId;
	double energy;
	double mass;
	double px;
	double py;
	double pz;
} Track;

#ifdef __cplusplus
}
#endif

#endif /* !TRACKS_H */

