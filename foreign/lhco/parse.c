// LHCO parser partly from lhco2lhe : credit Reboerto Franceschini (2010)

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "foreign.h"
#include "tracks.h"

#define LHCO_PARSE_LINE_BUF_SZ (1 << 9)
#define LHCO_PARSE_EVT_BUF_SZ (1 << 8)

float ptiso(float HADoverEM) {
	return floor(HADoverEM);
}

float etratio(float HADoverEM) {
	return HADoverEM - floor(HADoverEM);
}

float HoverEorpTiso(float HADoverEM, int pid) {
	if (pid == 13 || pid == -13)
		return ptiso(HADoverEM);
	else
		return HADoverEM;
}

int LHCOtoPDG(int lhcopid, double ntrack, double btag) {
	switch (lhcopid) {
	case 0: // photons
		return 22;
	case 1:
		// electrons (or positrons)
		if (ntrack > 0) return -11;
		else return 11;
	case 2: // muon
		if (ntrack > 0) return -13;
		else return 13;
	case 3: // tau
		if (ntrack > 0) return -15;
		else return 15;
	case 4: // jets
		if (btag == 2) return 55;
		else if (btag == 1) return -55;
		else return 211234;
	case 5: // mET
		return 121416;
	default:
		exit(1); // TODO
	}
}

struct lhco_particle {
	Int type; // LHCO, not PDG
	Float eta;
	Float phi;
	Float pt;
	Float mass;
	Float ntrk;
	Float btag;
	Float had_over_em;
};

struct lhco_event {
	short p_count;
	struct lhco_particle **parts;
};

Vec_Foreign LHCO_Input(const char *fname) {
	FILE *fin = fopen(fname, "r");
	if (!fin) exit(1); // TODO

	int evtbuf_sz = 40;
	struct lhco_event **events = calloc(evtbuf_sz, sizeof(struct lhco_event *));
	int nevent = 0;

	struct lhco_particle *parts = NULL;
	int npart;
	char buf[LHCO_PARSE_LINE_BUF_SZ];
	while (fgets(buf, LHCO_PARSE_LINE_BUF_SZ, fin)) {
		// Since no valid LHCO file will actually have lines longer
		// than LHCO_PARSE_LINE_BUF_SZ, we can just pretend that fgets
		// actually does give us entire lines
	
		// Now we separate the fields
		int fn = 0;
		char *field[15];
		char *fstart = buf, *fp;
		while (*fstart == ' ' || *fstart == '\t') fstart++;
		while (*fstart != '\n') {
			fp = fstart;
			while (*fp != ' ' && *fp != '\t' && *fp != '\n') fp++;
			field[fn] = fstart;
			fn++;
			if (*fp == '\n') {
				*fp = 0;
				break;
			}
			// since there's at least one byte of (irrelevant)
			// whitespace at fp, we can safely set it to 0.
			*fp = 0;
			fstart = fp+1;
			while (*fstart == ' ' || *fstart == '\t') fstart++;
		}

		// Parse
		if (field[0][0] == '#') // ignore the comment
			continue;
		if (field[0][0] == '0') {
			// new event
			if (parts) {
				// store the old one
				struct lhco_event *evt = malloc(sizeof(struct lhco_event));
				evt->p_count = npart;
				struct lhco_particle **ps = malloc(sizeof(struct lhco_particle *));
				*ps = parts;
				evt->parts = ps;
				// allocate new buffer if necessary
				if (nevent >= evtbuf_sz) {
					evtbuf_sz *= 2;
					events = realloc(events, evtbuf_sz * sizeof(struct lhco_event *));
				}
				events[nevent] = evt;
				nevent++;
			}
       			parts = calloc(LHCO_PARSE_EVT_BUF_SZ, sizeof(struct lhco_particle));
			npart = 0;
			continue;
		}
		// particle
		struct lhco_particle part = {
			atoi(field[1]), // type
			atof(field[2]), // eta
			atof(field[3]), // phi
			atof(field[4]), // pt
			atof(field[5]), // mass
			atof(field[6]), // ntrk
			atof(field[7]), // btag
			atof(field[8]), // had/em
		};
		parts[npart++] = part;
	}
	struct lhco_event *evt = malloc(sizeof(struct lhco_event));
	evt->p_count = npart;
	struct lhco_particle **ps = malloc(sizeof(struct lhco_particle *));
	*ps = parts;
	evt->parts = ps;
	// allocate new buffer if necessary
	if (nevent > evtbuf_sz) {
		evtbuf_sz *= 2;
		events = realloc(events, evtbuf_sz * sizeof(struct lhco_event *));
	}
	events[nevent] = evt;
	nevent++;
	if (!feof(fin)) exit(1); // TODO

	fclose(fin);

	Vec_Foreign result = {nevent, (Foreign *)events};
	return result;
}

Vec_Foreign LHCO_Parts(Foreign eventp) {
	struct lhco_event event = *(struct lhco_event *)eventp;
	Vec_Foreign result = {event.p_count, (Foreign *)event.parts};
	return result;
}

Foreign LHCO_Part_As_Track(Foreign f) {
	struct lhco_particle p = *(struct lhco_particle *)f;
	double theta = 2 * atan(exp(-2 * p.eta));
	Track *t = (Track *)malloc(sizeof(Track));
	t->pdgId = LHCOtoPDG(p.type, p.ntrk, p.btag);
	t->mass = p.mass;
	t->px = p.pt * sin(p.phi);
	t->py = p.pt * cos(p.phi);
	t->pz = p.pt * cos(theta);
	t->energy = sqrt(t->px*t->px + t->py*t->py + t->pz*t->pz + t->mass*t->mass);
	return t;
}

