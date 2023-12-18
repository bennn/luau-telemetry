artifact
===

This artifact packages the data for the paper:

 _Privacy-Respecting Type Error Telemetry at Scale_

URL
https://zenodo.org/doi/10.5281/zenodo.10275213

md5sum artifact.tar.gz
TODO

md5sum data.tar.gz
a62fa9e50258efda1756b6f85f71a799


### Getting Started

There are two files in the Zenodo record for this artifact:

* `data.tar.gz` has the original Luau telemetry data
* `artifact.tar.gz` has a result PDF, intermediate data, and scripts

The artifact code and the source for the paper are on GitHub:

* <https://github.com/bennn/luau-telemetry>

This artifact is primarily a **dataset**.
It shows how we reached the conclusions in the paper.
It includes the original data so that other researchers can study it
independently.

The scripts in this artifact are provided as-is for completeness.
We have not had time to polish them for other researchers.
They may have bugs. They may not work as advertised.


#### Artifact Overview

* `main.pdf` : reproduces figures from the paper and briefly explains their
  grounding in the data
* `main.tex` : source for `main.pdf`
* `Makefile` : run `make` to rebuild `main.pdf`
* `submission.pdf` : original submission to Programming 8.3
* `out/` : contains data and image files that `main.pdf` depends on
* `code/` : some scripts for processing original data and some scripts for
  processing data in the `out/` directory
* `data/` : empty directory, unpack `data.tar.gz` to fill it
* `README.md` : this file
* `Dockerfile` : basic Docker image to install Racket for the code scripts
* `type-error.rktd` : mapping from Luau type error codes to error names
* `runall.sh` : UNTESTED script to run all data-processing tasks, will take hours to finish


### Overview of Claims

The claims in the paper are grounded in its figures and tables.
If you trust the figures, then you can trust the claims.

Thus, the purpose of this artifact is to show how the figures
(in `main.pdf`) relate to the original data.

This artifact supports the following figures:

* Figure 3: Records per Hour
* Table 2: Size of Analyzed Code
* Table 3: Session Size
* Table 4: Current Type Errors and Background Errors
* Figure 4: Overview of Type Analysis Modes
* Figure 5: Type and Background Errors Grouped by Mode
* Table 5: Specific Errors in Edit Range
* Table 6: Type Error Popularity
  - Internal Limits, Code Too Complex
* Figure 6: Type Error Density
* Figure 7: Edits and Number of Errors


### Step-by-Step Instructions

To validate the central claims:

 1. Open `main.pdf` and compare its rudimentary figures and tables to the paper
    (`submission.pdf`)
 2. Compare the data in `main.pdf` to the data files in the `out/` folder
    that `main.pdf` references.

To explore the original data:

 1. Download and unpack `data.tar.gz`
 2. Open a few `.csv` files in a spreadsheet tool or text editor.


#### [Optional] Running the Code

First, install Racket v8.8 or later.

Either follow instructions on <https://download.racket-lang.org> or use Docker:

```
# prereq: start the docker daemon
docker build -t gjks-programming-2023 .
docker run -v "$PWD:/vol" -w /vol -ti gjks-programming-2023 bash
```

Unless you used Docker, install dependencies:

```
make install
```

Now you can run the Racket commands listed in `main.pdf`.

To run everything from the top (ETA 12 hours):

```
# prereq: download data.tar.gz from zenodo
tar -xzf data.tar.gz # unpack to data/ dir
sh runall.sh >& log.txt
```

The `runall` script will print status messages as it goes.


##### Example Run

We ran `runall.sh` in the Docker image on a single-user Linux machine
with 16 GB RAM.

The script printed 700 lines of output.

It spent 12 hours running `code/process.rkt` in various configurations:

- 2 hours to merge csv files (`--mode main`)
- 40 minutes to get info about code size and module switches (`--mode
  size-distro`, `modswitch`)
- 1 hours to group sessions (`--mode divide-sessions`)
- 7 hours to aggregate type errors (`--mode aggregate-te`)
- a few minutes total for other tasks (`--mode session-query`, `session-fold`,
  `count-te-editrange`)

It spent an additional 10 minutes running other scripts from the `code/`
directory to build figures and tables.

