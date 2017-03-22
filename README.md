wikitaxa
========



[![Build Status](https://api.travis-ci.org/ropensci/wikitaxa.svg?branch=master)](https://travis-ci.org/ropensci/wikitaxa)

`wikitaxa` - taxonomy data from Wikipedia/Wikidata/Wikispecies

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
res <- wiki_data("Poa annua")
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

## wikipedia


```r
pg <- get_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
res <- parse_wiki_page(pg)
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

## wikicommons


```r
pg <- get_wiki_page("https://commons.wikimedia.org/wiki/Abelmoschus")
res <- parse_wikicommons_page(pg)
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

## wikispecies


```r
pg <- get_wiki_page("https://species.wikimedia.org/wiki/Malus_domestica")
res <- parse_wikispecies_page(pg)
res$common_names[1:3]
#> [[1]]
#> NULL
#>
#> [[2]]
#> NULL
#>
#> [[3]]
#> NULL
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
