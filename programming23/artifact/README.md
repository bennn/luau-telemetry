
Ben's draft:
 https://zenodo.org/uploads/10275214

te = type error
fs = forced strict = background

* main.pdf = final output
* main.tex = TODO
* out/ = ready for "paper"
  - sessions.txt = 347598 sessions
  - overview.txt = 
  - size-distributions.rktd
  - summary-of-size-distributions.rktd
  - ss-*.rktd
* code/
  - row-distribution.rkt
  - sdupdate.rkt
  - size-distribution.rkt
* data.tar.gz = on zenodo
* render.rkt = ... recreate out/
  - 
* prepare.rkt = data to ... render inputs
  - unless out/full-dataset.csv exists, needs data/ files
    30min
  - overview, timelines
    30min each
* type-error.rkt = map Luau error codes to strings

[ ] where to submit?
   easychair 7730
   https://easychair.org/conferences/submission?submission=6738498&a=30700121&track=301404
[X] original data
[ ] data fixes from reviews???
  - sec 4.8 perhaps, 
[ ] ben-preprocessed data (cleaned?)
[ ] pdf for images, tables
[ ] ugly format tables
[ ] script with commands for everything which is ....
  - out/error-density-ss-nocheck.rktd
  - out/error-density-ss-nonstrict.rktd
  - out/error-density-ss-strict.rktd
  - 

  - out/row-distribution.pdf
  - out/lines-distribution.pdf
  - out/editrange-distribution.pdf
  - out/timespan-distribution.pdf
  - out/event-count-distribution.pdf
  - out/error-by-mode-te.pdf
  - out/error-by-mode-fs.pdf
  - out/error-count-nocheck-row--te-density-diff.pdf
  - out/error-count-nonstrict-row--te-density-diff.pdf
  - out/error-count-strict-row--te-density-diff.pdf
[ ] docker


- - -

MUST contain 3 parts

---

1. a Markdown-formatted README.md of your artifact,
2. a URL pointing to either
   - a single file containing the artifact (recommended)
   - or the address of a public source control repository,
3. a hash certifying the version of the artifact at submission time, either an
   sha256 hash of the single file (use the sha256sum command-line tool to
   generate the hash) or the full commit hash for the repository (e.g., from
   git reflog --no-abbrev)

The URL must be a public Google Drive, Dropbox, GitHub, Bitbucket, or GitLab
URL, to help protect the anonymity of the reviewers.
Artifacts do not need to be anonymous; reviewers will be aware of author identities.

public github: https://github.com/bennn/luau-telemetry
has paper and ... no data!

