// Use Fastjet
#include <fastjet/PseudoJet.hh>
#include <fastjet/ClusterSequence.hh>

#include <HepMC/IO_GenEvent.h>
#include <HepMC/GenEvent.h>
using namespace HepMC;
using namespace fastjet;
typedef GenEvent::particle_iterator particle_iter;

// Use libraries
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
using namespace std;


//const double pi = 3.1415926535898; // Pi number, DO NOT CHANGE
const double Etaall = 3.5; // Universal eta cut
const double Etael = 5.0;	// Eta cut on electron
const double Etamu = 5.0;	// Eta cut on muon
const double PTel = 0.0;	// Pt cut on electron
const double PTmu = 0.0;	// Pt cut on muon
const double isofrac = 0.9; //fraction of leptonic activity in the isolation cone
const double isorad = 0.4; // Lepton isolation radius
const double Rjet = 0.4; // Clustering radius
const double ptMinJet = 20.0; //Minimum pt for a jet
const double etaMaxReco = 3.0; // Eta cut on a jet
const double Rclust = 0.5;
#include "filter_open.hh"

#define uint unsigned int

///////////////////////////////////////

////////////////////////////////////////
// Define subroutines //////////////////
////////////////////////////////////////
double DR(const fastjet::PseudoJet &, const fastjet::PseudoJet &);

double signedDphi(const fastjet::PseudoJet &, const fastjet::PseudoJet &);

bool clusterTowers(const vector<fastjet::PseudoJet> &,
                   vector<fastjet::PseudoJet> &,
                   fastjet::ClusterSequence &, const double & );

int leptonsign(const fastjet::PseudoJet &);

bool lepton(const fastjet::PseudoJet &, const vector<fastjet::PseudoJet> &, 
	    const double, const double); 

int main(int argc, char** argv) {
  HepMC::IO_GenEvent fin("events.dat",std::ios::in);
  //Fastjet variables - partcle holders
  vector<fastjet::PseudoJet> hadrons;
  vector<fastjet::PseudoJet> preelectrons;
  vector<fastjet::PseudoJet> premuons;
  vector<fastjet::PseudoJet> isoelectrons;
  vector<fastjet::PseudoJet> isomuons;
  
  //Counters
  int base = 0;
  int med = 0;
  int highht = 0;
  int highmht = 0;
  
  // Counters for debugging purposes
  int lepf = 0;
  int fakemet = 0;
  int softevent = 0;
  int nojets = 0;
  int nomht = 0;
  int diff = 0;
  
  
  //Generate events
  uint iAbort = 0;
  string _evt;
  GenEvent *evt;
  while ((evt = fin.read_next_event())) {
    if (evt->event_number() % 100 == 0)
      cerr << evt->event_number() << endl;
    //Start clustering events
    hadrons.clear();
    preelectrons.clear();
    premuons.clear();
    isoelectrons.clear();
    isomuons.clear();
    //Good missing Et variables
    double misspx = 0.;
    double misspy = 0.;
    double missET = 0.;
    int goodjets = 0;
    double mhx = 0.0;
    double mhy = 0.0;
    double ht = 0.0;
    
    //Loop and sort over all final states
    for (particle_iter i = evt->particles_begin(); i != evt->particles_end() ; i++) {
      GenParticle *p = *i;
      int id = p->pdg_id();
      int idAbs = abs(id);
      FourVector m = p->momentum();
      double px=m.px(), py=m.py(), pz=m.pz(), e=m.e();
      fastjet::PseudoJet Part(px,
			      py,
			      pz,
			      e);
      Part.set_user_index(id);
      double pT = Part.perp();
      double eta = Part.pseudorapidity();
      if (p->status() != 1) continue;
      
      if (idAbs == 12 || idAbs == 14 ||
	  idAbs == 16 || idAbs == 1000039 ||
	  idAbs == 1000022) {
	misspx += px;
	misspy += py;
	continue;
      }
      
      //      missET = sqrt(misspx*misspx + misspy*misspy);
      
	    
      if( idAbs == 11) {
	preelectrons.push_back(Part);
	continue;
      }
      if( idAbs == 13) {
	premuons.push_back(Part);
	continue;
      }
      hadrons.push_back(Part);
    }

    missET = sqrt(misspx*misspx + misspy*misspy);

    // Identify isolated leptons and discard these events
    bool lepin = false;
    for (int iEl = 0; iEl < preelectrons.size(); iEl++){
      if (lepton(preelectrons[iEl], hadrons, 0.3, 0.2) && preelectrons[iEl].perp() > 10.0 &&
	  fabs(preelectrons[iEl].pseudorapidity()) < 2.5)
	lepin = true;
      else
	hadrons.push_back(preelectrons[iEl]);
    }
    for (int iMu = 0; iMu < premuons.size(); iMu++){
      if (lepton(premuons[iMu], hadrons, 0.3, 0.2) && premuons[iMu].perp() > 10.0 &&
	  fabs(premuons[iMu].pseudorapidity()) < 2.4)
	lepin = true;
      else
	hadrons.push_back(premuons[iMu]);
    }
    if (lepin)
      {lepf++; delete evt; continue;}
    // Cluster the event  into jets
    vector<fastjet::PseudoJet> sortedCMSJets;
    fastjet::ClusterSequence CMSSeq;
    bool CMSworked = clusterTowers(hadrons, sortedCMSJets, CMSSeq, Rclust);
    if (!CMSworked)
      {nojets++; delete evt; continue;}
    // Calculate mht, ht, number of good jets
    for (int iH = 0; iH < sortedCMSJets.size(); iH++){
      //if (sortedCMSJets[iH].perp() > 30.0){
      mhx += sortedCMSJets[iH].px();
      mhy += sortedCMSJets[iH].py();
      // }
      if (sortedCMSJets[iH].perp() > 50.0 && fabs(sortedCMSJets[iH].rap()) < 2.5){
	ht += sortedCMSJets[iH].perp();
	goodjets++;
      }
    }
    fastjet::PseudoJet mht_jet(-mhx, -mhy, 0, 0);
    double mht = sqrt(mhx*mhx + mhy*mhy);
    if ((mht/missET < 0.1) || (mht/missET > 10.0))
      diff++;
    if (fabs(signedDphi(mht_jet, sortedCMSJets[0])) < 0.5 || 
	fabs(signedDphi(mht_jet, sortedCMSJets[1])) < 0.5 ||
	fabs(signedDphi(mht_jet, sortedCMSJets[2])) < 0.3 || goodjets < 3)
      {fakemet++; delete evt; continue;}
    if(ht < 350)
      softevent++;
    if (mht < 200)
      nomht++;
    if (ht > 350 && mht > 200)
      base++;
    if (ht > 500 && mht > 350)
      med++;
    if (ht > 800 && mht > 200)
      highht++;
    if(mht > 500 && ht > 350)
      highmht++; 
    delete evt;
  }
  //cout << diff << " events with significant descrepancy btw missing et and HT" << endl;
  //cout << nojets << " events in which the clustering failed" << endl;
  //cout << lepf << " events failed lepton veto" << endl;
  //cout << fakemet << " events failed due to fake met rejection" << endl;
  //cout << softevent << " events failed since they are too soft" << endl;
  //cout << nomht << " events do not have sufficient mssing ht" << endl;
  cout << "Baseline events selected " << base << endl;
  cout << "Events in medium region " << med << endl;
  cout << "Events in high ht region " << highht << endl;
  cout << "Events in hight mht region " << highmht << endl;  
  return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////
////// Lepton isolation routine //////////////////////////////////////////////////////////////
bool lepton(const fastjet::PseudoJet & jet1,
	    const vector <fastjet::PseudoJet> & hads,
	    const double rad, const double leptiso) {
  double lpt = jet1.perp();
  double hpt = 0;
  double het = 0;
  for (uint i=0; i<hads.size(); i++) {
    if (DR(jet1, hads[i])<rad)
      het += hads[i].perp();
  }
  if (het/lpt < leptiso)
    return true;
  return false;
}

