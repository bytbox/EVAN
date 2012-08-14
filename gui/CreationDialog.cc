#include "canvas.hh"
#include "dialog.hh"

#include <QtGui>

CreationDialog::CreationDialog(DialogFields *fields) : fields(fields) {
	setWindowTitle(tr(("Create " + fields->title).c_str()));

	okButton = new QPushButton(tr("Ok"));
	cancelButton = new QPushButton(tr("Cancel"));
	buttonLayout.addWidget(okButton);
	buttonLayout.addWidget(cancelButton);
	connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));
	connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
	layout.addWidget(fields);
	layout.addLayout(&buttonLayout);
	setLayout(&layout);
}

CreationDialog::~CreationDialog() {
	delete okButton;
	delete cancelButton;
}

