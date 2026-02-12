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
```

* I have removed the dependency `WikidataR`, which depended on `WikidataQueryServiceR`.
