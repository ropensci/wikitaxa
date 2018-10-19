wikitaxa
========



[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran checks](https://cranchecks.info/badges/worst/wikitaxa)](https://cranchecks.info/pkgs/wikitaxa)
[![Build Status](https://api.travis-ci.org/ropensci/wikitaxa.svg?branch=master)](https://travis-ci.org/ropensci/wikitaxa)
[![codecov](https://codecov.io/gh/ropensci/wikitaxa/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/wikitaxa)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/wikitaxa)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/wikitaxa)](https://cran.r-project.org/package=wikitaxa)

`wikitaxa` - taxonomy data from Wikipedia/Wikidata/Wikispecies

Get started below and with the vignette: <https://cran.r-project.org/web/packages/wikitaxa/vignettes/wikitaxa_vignette.html>

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
install.packages("devtools")
devtools::install_github("ropensci/wikitaxa")
```


```r
library('wikitaxa')
```

## wiki data


```r
wt_data("Poa annua")
```

Get a Wikidata ID


```r
wt_data_id("Mimulus foliatus")
#> [1] "Q6495130"
#> attr(,"class")
#> [1] "wiki_id"
```

## wikipedia

lower level


```r
pg <- wt_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
res <- wt_wiki_page_parse(pg)
res$iwlinks
#> [1] "https://commons.wikimedia.org/wiki/Category:apples"         
#> [2] "https://commons.wikimedia.org/wiki/Category:Apple_cultivars"
#> [3] "https://www.wikidata.org/wiki/Q158657"                      
#> [4] "https://www.wikidata.org/wiki/Q18674606"                    
#> [5] "https://species.wikimedia.org/wiki/Malus_pumila"            
#> [6] "https://species.wikimedia.org/wiki/Malus_domestica"
```

higher level


```r
res <- wt_wikipedia("Malus domestica")
res$common_names
#> # A tibble: 1 x 2
#>   name  language
#>   <chr> <chr>   
#> 1 Apple en
res$classification
#> # A tibble: 3 x 2
#>   rank       name        
#>   <chr>      <chr>       
#> 1 plainlinks ""          
#> 2 species    M. pumila   
#> 3 binomial   Malus pumila
```

choose a wikipedia language


```r
# French
wt_wikipedia(name = "Malus domestica", wiki = "fr")
# Slovak
wt_wikipedia(name = "Malus domestica", wiki = "sk")
# Vietnamese
wt_wikipedia(name = "Malus domestica", wiki = "vi")
```


## wikicommons

lower level


```r
pg <- wt_wiki_page("https://commons.wikimedia.org/wiki/Abelmoschus")
res <- wt_wikicommons_parse(pg)
res$common_names[1:3]
#> [[1]]
#> [[1]]$name
#> [1] "okra"
#> 
#> [[1]]$language
#> [1] "en"
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "مسكي"
#> 
#> [[2]]$language
#> [1] "ar"
#> 
#> 
#> [[3]]
#> [[3]]$name
#> [1] "Abelmoş"
#> 
#> [[3]]$language
#> [1] "az"
```

higher level


```r
res <- wt_wikicommons("Abelmoschus")
res$classification
#> # A tibble: 15 x 2
#>    rank       name            
#>    <chr>      <chr>           
#>  1 Domain     Eukaryota       
#>  2 unranked   Archaeplastida  
#>  3 Regnum     Plantae         
#>  4 Cladus     angiosperms     
#>  5 Cladus     eudicots        
#>  6 Cladus     core eudicots   
#>  7 Cladus     superrosids     
#>  8 Cladus     rosids          
#>  9 Cladus     eurosids II     
#> 10 Ordo       Malvales        
#> 11 Familia    Malvaceae       
#> 12 Subfamilia Malvoideae      
#> 13 Tribus     Hibisceae       
#> 14 Genus      Abelmoschus     
#> 15 Authority  " Medik. (1787)"
res$common_names
#> # A tibble: 19 x 2
#>    name             language
#>    <chr>            <chr>   
#>  1 okra             en      
#>  2 مسكي             ar      
#>  3 Abelmoş          az      
#>  4 Ibiškovec        cs      
#>  5 Bisameibisch     de      
#>  6 Okrat            fi      
#>  7 Abelmosco        gl      
#>  8 Abelmošus        hr      
#>  9 Ybiškė           lt      
#> 10 അബെൽമോസ്കസ്        ml      
#> 11 Абельмош         mrj     
#> 12 Abelmoskusslekta nn      
#> 13 Piżmian          pl      
#> 14 Абельмош         ru      
#> 15 موري             sd      
#> 16 Okrasläktet      sv      
#> 17 Абельмош         udm     
#> 18 Chi Vông vang    vi      
#> 19 黄葵属           zh
```

## wikispecies

lower level


```r
pg <- wt_wiki_page("https://species.wikimedia.org/wiki/Malus_domestica")
res <- wt_wikispecies_parse(pg, types = "common_names")
res$common_names[1:3]
#> [[1]]
#> [[1]]$name
#> [1] "Ябълка"
#> 
#> [[1]]$language
#> [1] "български"
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "Poma, pomera"
#> 
#> [[2]]$language
#> [1] "català"
#> 
#> 
#> [[3]]
#> [[3]]$name
#> [1] "jabloň domácí"
#> 
#> [[3]]$language
#> [1] "čeština"
```

higher level


```r
res <- wt_wikispecies("Malus domestica")
res$classification
#> # A tibble: 8 x 2
#>   rank        name         
#>   <chr>       <chr>        
#> 1 Superregnum Eukaryota    
#> 2 Regnum      Plantae      
#> 3 Cladus      Angiosperms  
#> 4 Cladus      Eudicots     
#> 5 Cladus      Core eudicots
#> 6 Cladus      Rosids       
#> 7 Cladus      Eurosids I   
#> 8 Ordo        Rosales
res$common_names
#> # A tibble: 22 x 2
#>    name          language 
#>    <chr>         <chr>    
#>  1 Ябълка        български
#>  2 Poma, pomera  català   
#>  3 jabloň domácí čeština  
#>  4 Apfel         Deutsch  
#>  5 Aed-õunapuu   eesti    
#>  6 Μηλιά         Ελληνικά 
#>  7 Apple         English  
#>  8 Manzano       español  
#>  9 Pomme         français 
#> 10 Melâr         furlan   
#> # ... with 12 more rows
```

## Contributors

* [Ethan Welty](https://github.com/ezwelty)
* [Scott Chamberlain](https://github.com/sckott)

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/wikitaxa/issues).
* License: MIT
* Get citation information for `wikitaxa` in R doing `citation(package = 'wikitaxa')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[![ropensci](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
