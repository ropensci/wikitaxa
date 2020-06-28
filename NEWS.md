wikitaxa 0.4.0
==============

### MINOR IMPROVEMENTS

* remove docs link to httr that's not used in package (#19)
* remove egs from readme, change vignette name


wikitaxa 0.3.0
==============

### MINOR IMPROVEMENTS

* integration with vcr for test caching for all HTTP requests (#17) (#18)
* link to `taxize` book and `wikitaxa` vignette in readme (#16)

### BUG FIXES

* fix to `wt_wikipedia()` to separate `<br>` tags appropriately (#15)


wikitaxa 0.2.0
==============

### BUG FIXES

* `wt_wikicommons()` fails better now when a page does not exist, and is now consitent with the rest of package (#14)
* `wt_wikicommons()` fixed - classification objects were not working correctly as the data used is a hot mess - tried to improve parsing of that text (#13)
* `wt_data()` fix - was failing due to i think a change in the internal pkg `WikidataR` (#12)


wikitaxa 0.1.4
==============

### NEW FEATURES

* `wt_wikipedia()` and `wt_wikipedia_search()` gain parameter `wiki`
to give the wiki language, which defaults to `en` (#9)

### MINOR IMPROVEMENTS

* move some examples to dontrun (#11)


wikitaxa 0.1.0
==============

### NEW FEATURES

* Released to CRAN
