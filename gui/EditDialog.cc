#include "canvas.hh"
#include "dialog.hh"

EditDialog::EditDialog(DialogFields *fields) : fields(fields) {
	setWindowTitle(tr("Create"));

	okButton = new QPushButton(tr("Ok"));
	cancelButton = new QPushButton(tr("Cancel"));
	buttonLayout.addWidget(okButton);
	buttonLayout.addWidget(cancelButton);
	connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));
	connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
	layout.addLayout(&buttonLayout);
	setLayout(&layout);
}

EditDialog::~EditDialog() {
	delete okButton;
	delete cancelButton;
}

