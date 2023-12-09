artifact
===

This artifact packages the data for the paper:

 _Privacy-Respecting Type Error Telemetry at Scale_

URL
https://doi.org/10.5281/zenodo.10275214

md5sum artifact.tar.gz
28e1d1451d549868698959cc42a4c869


Instructions:

 1. Open `main.pdf`
 2. Verify that the figures and tables match the paper (`submission.pdf`)
 3. For each section of `main.pdf`, check that the data it reports
    matches the data file(s) it refers to.
 4. (Optional) Run scripts in the `code/` directory, run `make` to rebuild
    the pdf, and check that the pdf did not change.
 5. (Optional) Download `data.tar.gz` from Zenodo and explore the data
    using your favorite text editor or spreadsheet.

Warning: running `prepare.rkt` may take hours. Some of its commands process
the entire 1.5 million records.

## Organization

data in out/
input and output


## Install

Docker 

start docker daemon

docker build -t gjks-programming-2023 .
docker run -v "$PWD:/vol" -w /vol -ti gjks-programming-2023 bash

Racket

Tested on v8.10

Deps:

 `make install`

	raco pkg install --auto text-table pict-abbrevs gregor gtp-util colorblind-palette

