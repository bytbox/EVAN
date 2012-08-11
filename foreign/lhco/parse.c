// LHCO parser partly from lhco2lhe : credit Reboerto Franceschini (2010)

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "foreign.h"

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
};

Vec_Foreign LHCO_Input(const char *fname) {
	FILE *fin = fopen(fname, "r");
	if (!fin) exit(1); // TODO

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
			// since there's at least one byte of (irrelevant)
			// whitespace at fp, we can safely set it to 0.
			*fp = 0;
			fstart = fp;
			while (*fstart == ' ' || *fstart == '\t') fstart++;
			fn++;
		}

		// Parse
		if (field[0][0] == '#') // ignore the comment
			continue;
		if (field[0][0] == '0') {
			// new event
			if (parts) {
				// store the old one
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
	if (!feof(fin)) exit(1); // TODO

	fclose(fin);

	Vec_Foreign result = {0, NULL};
	return result;
}

/*
int main(int argc, char* argv[]) {

	int ninout = 0;
	vector < string > fields;

	string infilename = argv[1];
	string outfilename;

	ifstream unw(argv[1]);
	ofstream lhe;

	stringstream eventparticles;
	string temp;
	string tempfield;
	istringstream infield;
	istringstream instream;
	int blockposition = 0;
	string in_pid;
	double eta, phi, pt, jmas, ntrk, btag, HADoverEM;
	int pid;
	double pX, pY, pL, m, E;
	bool write_event = false;

	//LOOP on LINES
	ninout = 0;
	while (getline(unw, temp)) {

		instream.clear();
		instream.str(temp);
		fields.clear();
		while (getline(instream, tempfield, ' ')) {

			if (tempfield != "" && tempfield != " ") {
				fields.push_back(tempfield);
				//cout<<tempfield<<endl;
			}
		}

		// now I know the content of the line
		// cout << "read fields: " << fields.size() << endl;
		//cout << "first field: " << fields[0] << endl;

		if (fields[0].substr(0, 1) == "#") {
			//cout << "was a comment" << endl;
			// this is a comment line, do nothing

		} else if (fields[0] == "0") {
			//this is the trigger line of the event, reset the counter of the particles and
			// ask to write the particles that are waiting to be written
			//cout<<" trigger line"<<endl;

			write_event = true;

		} else {
			//cout<<"Particle line"<<endl;

			// this line is a particle, store it in the container
			ninout++;
			eta = atof(fields[2].c_str());
			phi = atof(fields[3].c_str());
			pt = atof(fields[4].c_str());
			m = atof(fields[5].c_str());
			ntrk = floor(atof(fields[6].c_str())); //some objects have non integer number of tracks
			btag = atof(fields[7].c_str());
			HADoverEM = atof(fields[8].c_str());

			pid = LHCOtoPDG(fields[1], ntrk, btag);

			HoverEorpTiso(HADoverEM, pid);

			double p;
			double theta = 2 * atan(exp(-2 * eta));
			p = pt / sin(theta);

			pX = pt * sin(phi);
			pY = pt * cos(phi);
			pL = p * cos(theta);

			E = sqrt(pX * pX + pY * pY + pL * pL + m * m);
			if (false) {
				eventparticles << "    " << pid << " 1 " << btag << " " << ntrk
						<< " 0  0 " << pX << " " << pY << " " << pL << " " << E
						<< "  " << m << " " << HoverEorpTiso(HADoverEM, pid)
						<< "  " << etratio(HADoverEM) << endl;
			}
			eventparticles << "    " << pid << " 1 " << btag << " " << ntrk
					<< " 0  0 " << pX << " " << pY << " " << pL << " " << E
					<< "  " << m << " " << "0"
					<< "  " << "0"<< endl;
			//eventparticles << "    " << pid << " 1 " << btag << " " << ntrk
			//		<< " 0  0 " << pX << " " << pY << " " << pL << " " << E
			//		<< "  " << m << " " << HoverEorpTiso(HADoverEM, pid)
			//		<< "  " << etratio(HADoverEM)<< endl;
		}

		if ((eventparticles.str()).size() >= 1 && write_event == true) {
			// write the event and reset the flag
			//cout<< "writing event"<<endl;
			lhe.open(outfilename.c_str(), ios::app);
			lhe << "<event>\n" << ninout
					<< " 0  1.0  0.9118800E+02  0.7816531E-02  0.1300000E+00"
					<< endl;
			ninout = 0;
			lhe << eventparticles.str();
			eventparticles.str("");
			//cout<<"now the particle vector is "<<eventparticles.str().size()<<endl;

			lhe << "</event>" << endl;
			lhe.close();

		}

		write_event = false;

	}
	return 0;
}
*/
