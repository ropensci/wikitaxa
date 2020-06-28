wikitaxa
========



[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran checks](https://cranchecks.info/badges/worst/wikitaxa)](https://cranchecks.info/pkgs/wikitaxa)
[![R-check](https://github.com/ropensci/wikitaxa/workflows/R-check/badge.svg)](https://github.com/ropensci/wikitaxa/actions/)
[![codecov](https://codecov.io/gh/ropensci/wikitaxa/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/wikitaxa)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/wikitaxa)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/wikitaxa)](https://cran.r-project.org/package=wikitaxa)

`wikitaxa` - taxonomy data from Wikipedia/Wikidata/Wikispecies

Get started below and with the vignette: <https://cran.r-project.org/package=wikitaxa>

See also the taxize book: <https://ropensci.github.io/taxize-book/>


### Low level API

The low level API is meant for power users and gives you more control,
but requires more knowledge.

* `wt_wiki_page()`
* `wt_wiki_page_parse()`
* `wt_wiki_url_build()`
* `wt_wiki_url_parse()`
* `wt_wikispecies_parse()`
* `wt_wikicommons_parse()`
* `wt_wikipedia_parse()`

### High level API

The high level API is meant to be easier and faster to use.

* `wt_data()`
* `wt_data_id()`
* `wt_wikispecies()`
* `wt_wikicommons()`
* `wt_wikipedia()`

Search functions:

* `wt_wikicommons_search()`
* `wt_wikispecies_search()`
* `wt_wikipedia_search()`

## Installation

CRAN version


```r
install.packages("wikitaxa")
```

Dev version


```r
remotes::install_github("ropensci/wikitaxa")
```


```r
library('wikitaxa')
```

## Contributors

* [Ethan Welty](https://github.com/ezwelty)
* [Scott Chamberlain](https://github.com/sckott)

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/wikitaxa/issues).
* License: MIT
* Get citation information for `wikitaxa` in R doing `citation(package = 'wikitaxa')`
* Please note that this project is released with a [Contributor Code of Conduct][coc]. By participating in this project you agree to abide by its terms.

[![ropensci](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[coc]: https://github.com/ropensci/wikitaxa/blob/master/CODE_OF_CONDUCT.md
