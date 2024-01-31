all: full

full: fast
	bibtex paper
	pdflatex paper
	pdflatex paper

fast:
	pdflatex paper

export: clean
	cd .. ; zip -r programming23.zip programming23/AGREEMENT.pdf programming23/Makefile programming23/README.md programming23/bib.bib programming23/code programming23/img programming23/ldiff.cfg programming23/out programming23/paper.aux programming23/paper.bbl programming23/paper.blg programming23/paper.log programming23/paper.pdf programming23/paper.tex programming23/programming.cls programming23/telemetry-submission.pdf ; cd -

clean:
	find . -name "compiled" | xargs rm -r

