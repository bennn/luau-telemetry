
Ben's draft:
 https://zenodo.org/uploads/10275214

te = type error
fs = forced strict = background

* main.pdf = final output
* main.tex = TODO
* out/ = ready for "paper"
* data.tar.gz = on zenodo
* render.rkt = ... recreate img/
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
[ ] code
  - code/error-count
[ ] script with commands for everything which is ....
  - out/error-density-ss-nocheck.rktd
  - out/error-density-ss-nonstrict.rktd
  - out/error-density-ss-strict.rktd
  - 

  - img/row-distribution.pdf
  - img/lines-distribution.pdf
  - img/editrange-distribution.pdf
  - img/timespan-distribution.pdf
  - img/event-count-distribution.pdf
  - img/error-by-mode-te.pdf
  - img/error-by-mode-fs.pdf
  - img/error-count-nocheck-row--te-density-diff.pdf
  - img/error-count-nonstrict-row--te-density-diff.pdf
  - img/error-count-strict-row--te-density-diff.pdf
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

