all:
	pdflatex main

install:
	raco pkg install --auto text-table pict-abbrevs gregor gtp-util colorblind-palette

export: clean
	rm -f data/*.csv
	rm -f out/full-dataset.csv
	cd out ; tar -xzf error-density-ss-nocheck.rktd.tar.gz ; rm error-density-ss-nocheck.rktd.tar.gz ; tar -xzf ss-nocheck.rktd.tar.gz; rm ss-nocheck.rktd.tar.gz; tar -xzf ss-nonstrict.rktd.tar.gz; rm ss-nonstrict.rktd.tar.gz ; cd ..
	cd .. ; tar -czf artifact.tar.gz artifact ; cd -

cleandata:
	rm -f data/*.csv
	rm -f out/full-dataset.csv
	rm -f out/error-density-ss-nocheck.rktd
	rm -f out/ss-nocheck.rktd
	rm -f out/ss-nonstrict.rktd
	rm -f main.aux main.log texput.log

clean:
	find . -name "compiled" | xargs rm -r

