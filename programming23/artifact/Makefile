all:
	pdflatex main

install:
	raco pkg install --auto text-table pict-abbrevs gregor gtp-util colorblind-palette

export: clean cleandata
	cd out ; for X in out/error-density-ss-nocheck.rktd out/ss-nocheck.rktd out/ss-nonstrict.rktd ; do tar -czf ${X}.tar.gz ${X} ; rm ${X} ; done ; cd ..
	cd .. ; tar -czf artifact.tar.gz artifact ; cd -

cleandata:
	rm -f data/*.csv
	rm -f out/full-dataset.csv
	rm -f out/error-density-ss-nocheck.rktd
	rm -f out/ss-nocheck.rktd
	rm -f out/ss-nonstrict.rktd

clean:
	find . -name "compiled" | xargs rm -r

