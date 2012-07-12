#include "app.hh"

MainPanel::MainPanel() {
	mainLayout.addWidget(&canvas);
	outsideLayout.addLayout(&mainLayout);
	setLayout(&outsideLayout);
}

