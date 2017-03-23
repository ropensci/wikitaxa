wikitaxa
========



[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://api.travis-ci.org/ropensci/wikitaxa.svg?branch=master)](https://travis-ci.org/ropensci/wikitaxa)
[![codecov](https://codecov.io/gh/ropensci/wikitaxa/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/wikitaxa)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/wikitaxa)](https://github.com/metacran/cranlogs.app)

`wikitaxa` - taxonomy data from Wikipedia/Wikidata/Wikispecies


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

## Installation


```r
install.packages("devtools")
devtools::install_github("ropensci/wikitaxa")
```


```r
library('wikitaxa')
```

## wiki data


```r
res <- wt_data("Poa annua")
res$descriptions
#>   language                              value
#> 1       de Art der Gattung Rispengräser (Poa)
#> 2       fa                      گونه‌ای از چبر
#> 3       en                   species of plant
#> 4       bg                       вид растение
#> 5       nl plantensoort uit de grassenfamilie
#> 6       ar                    نوع من النباتات
#> 7       hy                     բույսերի տեսակ
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
#> [[1]]
#> [1] "https://en.wiktionary.org/wiki/apple"
#> 
#> [[2]]
#> [1] "https://commons.wikimedia.org/wiki/Special:Search/Apple"
#> 
#> [[3]]
#> [1] "https://en.wikiquote.org/wiki/Apples"
#> 
#> [[4]]
#> [1] "https://en.wikisource.org/wiki/1911_Encyclop%C3%A6dia_Britannica/Apple"
#> 
#> [[5]]
#> [1] "https://en.wikibooks.org/wiki/Apples"
#> 
#> [[6]]
#> [1] "https://species.wikimedia.org/wiki/Malus_domestica"
#> 
#> [[7]]
#> [1] "https://commons.wikimedia.org/wiki/Category:Apple_cultivars"
```

higher level


```r
res <- wt_wikipedia("Malus domestica")
res$common_names
#>         name language
#> 1 apple tree       en
#> 2      apple       en
#> 3      Apple       en
res$classification
#>       rank         name
#> 1  kingdom      Plantae
#> 2 unranked  Angiosperms
#> 3 unranked     Eudicots
#> 4 unranked       Rosids
#> 5    order      Rosales
#> 6   family     Rosaceae
#> 7    genus        Malus
#> 8  species    M. pumila
#> 9 binomial Malus pumila
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
#>            rank           name
#> 1        Domain      Eukaryota
#> 2    • unranked Archaeplastida
#> 3      • Regnum        Plantae
#> 4      • Cladus    angiosperms
#> 5      • Cladus       eudicots
#> 6      • Cladus  core eudicots
#> 7      • Cladus    superrosids
#> 8      • Cladus         rosids
#> 9      • Cladus    eurosids II
#> 10       • Ordo       Malvales
#> 11    • Familia      Malvaceae
#> 12 • Subfamilia     Malvoideae
#> 13     • Tribus      Hibisceae
#> 14            •    Abelmoschus
#> 15               Medik. (1787)
res$common_names
#>             name language
#> 1           okra       en
#> 2           مسكي       ar
#> 3        Abelmoş       az
#> 4      Ibiškovec       cs
#> 5   Bisameibisch       de
#> 6          Okrat       fi
#> 7      Abelmosco       gl
#> 8      Abelmošus       hr
#> 9         Ybiškė       lt
#> 10  അബെ\u0d7dമോസ്കസ്       ml
#> 11      Абельмош      mrj
#> 12       Piżmian       pl
#> 13      Абельмош       ru
#> 14          موري       sd
#> 15   Okrasläktet       sv
#> 16      Абельмош      udm
#> 17 Chi Vông vang       vi
#> 18        黄葵属       zh
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
#> [1] "Apfel"
#> 
#> [[3]]$language
#> [1] "Deutsch"
```

higher level


```r
res <- wt_wikispecies("Malus domestica")
res$classification
#>          rank          name
#> 1 Superregnum     Eukaryota
#> 2      Regnum       Plantae
#> 3      Cladus   Angiosperms
#> 4      Cladus      Eudicots
#> 5      Cladus Core eudicots
#> 6      Cladus        Rosids
#> 7      Cladus    Eurosids I
#> 8        Ordo       Rosales
res$common_names
#>               name   language
#> 1           Ябълка  български
#> 2     Poma, pomera     català
#> 3            Apfel    Deutsch
#> 4      Aed-õunapuu      eesti
#> 5            Μηλιά   Ελληνικά
#> 6            Apple    English
#> 7          Manzano    español
#> 8            Pomme   français
#> 9            Melâr     furlan
#> 10        사과나무     한국어
#> 11          ‘Āpala    Hawaiʻi
#> 12            Melo   italiano
#> 13           Aapel Nordfriisk
#> 14  Maçã, Macieira  português
#> 15 Яблоня домашняя    русский
#> 16   Tarhaomenapuu      suomi
#> 17            Elma     Türkçe
#> 18  Яблуня домашня українська
#> 19          Pomaro     vèneto
```

## Contributors

* [Ethan Welty](https://github.com/ezwelty)
* [Scott Chamberlain](https://github.com/sckott)

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/wikitaxa/issues).
* License: MIT
* Get citation information for `wikitaxa` in R doing `citation(package = 'wikitaxa')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![ropensci](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
