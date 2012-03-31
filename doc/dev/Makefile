TEXFILES = dev-manual.tex tohs.tex

all: dev-manual.pdf

dev-manual.pdf: ${TEXFILES}
	pdflatex $<
	pdflatex $<

clean:
	${RM} *.aux *.log *.pdf *.toc

.PHONY: all clean

