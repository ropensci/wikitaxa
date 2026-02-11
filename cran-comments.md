## Test environments and check results

### local: Pop!_OS 22.04 LTS, R 4.5.2 

0 errors | 0 warnings | 0 notes

### `macos-latest`, `ubuntu-latest`, and `windows-latest` on Github actions

0 errors | 0 warnings | 0 notes

See results at: https://github.com/ropensci/wikitaxa/actions

### R-devel with win-builder.r-project.org

0 errors | 0 warnings | 1 notes

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Zachary Foster <zacharyfoster1989@gmail.com>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2026-02-08 as requires archived package
    'WikidataQueryServiceR'

Found the following (possibly) invalid URLs:
  URL: https://cloud.r-project.org/web/checks/check_results_wikitaxa.html
    From: README.md
    Status: 404
    Message: Not Found
    CRAN URL not in canonical form
  URL: https://docs.ropensci.org/wikitaxa (moved to https://docs.ropensci.org/wikitaxa/)
    From: DESCRIPTION
          man/wikitaxa-package.Rd
    Status: 301
    Message: Moved Permanently
Canonical CRAN.R-project.org URLs use https.
For content that is 'Moved Permanently', please change http to https,
add trailing slashes, or replace the old by the new URL.
```

* I have removed the dependency `WikidataR`, which depended on `WikidataQueryServiceR`.
* I think "https://cloud.r-project.org/web/checks/check_results_wikitaxa.html" should be valid once published
* "https://docs.ropensci.org/wikitaxa/" is already the URL used (trailing slash included), so I dont understand why this note exists
