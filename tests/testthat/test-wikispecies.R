context("wt_wikispecies")

test_that("wt_wikispecies returns non-empty results", {
  skip_on_cran()

  vcr::use_cassette(
    "wt_wikispecies_malus",
    {
      aa <- wt_wikispecies(name = "Malus domestica")
    },
    preserve_exact_body_bytes = TRUE
  )

  expect_is(aa, "list")
  expect_named(
    aa,
    c('langlinks', 'externallinks', 'common_names', 'classification')
  )
  expect_is(aa$langlinks, "data.frame")
  expect_is(aa$externallinks, "character")
  expect_is(aa$common_names, "data.frame")
  # expect_named(aa$common_names, c('name', 'language'))
  expect_is(aa$classification, "data.frame")
  expect_named(aa$classification, c('rank', 'name'))
})

test_that("wt_wikispecies returns non-empty results", {
  vcr::use_cassette(
    "wt_wikispecies_poa",
    {
      bb <- wt_wikispecies(name = "Poa annua")
    },
    preserve_exact_body_bytes = TRUE
  )

  expect_is(bb, "list")
  expect_named(
    bb,
    c('langlinks', 'externallinks', 'common_names', 'classification')
  )
  expect_is(bb$langlinks, "data.frame")
  expect_is(bb$externallinks, "character")
  expect_is(bb$common_names, "data.frame")
  # expect_named(bb$common_names, c('name', 'language'))
  expect_is(bb$classification, "data.frame")
  expect_named(bb$classification, c('rank', 'name'))
})

test_that("wt_wikispecies fails well", {
  skip_on_cran()

  expect_error(wt_wikispecies(), "argument \"name\" is missing")
  expect_error(wt_wikispecies(5), "name must be of class character")

  # "name" must be length 1
  expect_error(
    wt_wikispecies(c("Pinus", "asdfadsf")),
    "length\\(name\\) == 1 is not TRUE"
  )

  # "utf8" must be logical
  expect_error(
    wt_wikispecies("Pinus", "asdf"),
    "utf8 must be of class logical"
  )
})

context("wt_wikispecies_parse")

test_that("wt_wikispecies_parse returns non-empty results", {
  skip_on_cran()

  vcr::use_cassette(
    "wt_wikispecies_parse",
    {
      url <- "https://species.wikimedia.org/wiki/Malus_domestica"
      pg <- wt_wiki_page(url)
      types <- c("common_names")
      result <- wt_wikispecies_parse(pg, types = types)
    },
    preserve_exact_body_bytes = TRUE
  )

  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})

# FIXME: utf=FALSE for now until curl::curl_escape fix
# https://github.com/jeroen/curl/issues/228
context("wt_wikispecies_search")

test_that("wt_wikispecies_search works", {
  skip_on_cran()

  vcr::use_cassette("wt_wikispecies_search", {
    aa <- wt_wikispecies_search(query = "Pinus", utf8 = FALSE)
  })

  expect_is(aa, "list")
  expect_is(aa$continue, "list")
  expect_is(aa$query, "list")
  expect_is(aa$query$searchinfo, "list")
  expect_is(aa$query$search, "data.frame")
  expect_named(
    aa$query$search,
    c('ns', 'title', 'pageid', 'size', 'wordcount', 'snippet', 'timestamp')
  )
})

# no results when not found
test_that("wt_wikispecies_search_not_found", {
  vcr::use_cassette("wt_wikispecies_search_not_found", {
    x <- wt_wikispecies_search("asdfadfaadfadfs", utf8 = FALSE)
  })
  expect_equal(NROW(x$query$search), 0)
})

test_that("wt_wikispecies_search fails well", {
  skip_on_cran()

  expect_error(
    wt_wikispecies_search(),
    "argument \"query\" is missing"
  )
  expect_error(
    wt_wikispecies_search("Pinus", limit = "adf"),
    "limit must be of class integer, numeric"
  )
  expect_error(
    wt_wikispecies_search("Pinus", offset = "adf"),
    "offset must be of class integer, numeric"
  )
})
