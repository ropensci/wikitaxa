context("wt_wikipedia")

vcr::use_cassette("wt_wikipedia_malus", {
  test_that("wt_wikipedia returns non-empty results", {
    skip_on_cran()

    aa <- wt_wikipedia(name = "Malus domestica")

    expect_is(aa, "list")
    expect_named(aa, c('langlinks', 'externallinks', 'common_names',
                       'classification', 'synonyms'))
    expect_is(aa$langlinks, "data.frame")
    expect_is(aa$externallinks, "character")
    expect_is(aa$common_names, "data.frame")
    expect_named(aa$common_names, c('name', 'language'))
    expect_is(aa$classification, "data.frame")
    expect_named(aa$classification, c('rank', 'name'))
  })
})

vcr::use_cassette("wt_wikipedia_poa", {
  test_that("wt_wikipedia returns non-empty results", {
    bb <- wt_wikipedia(name = "Poa annua")

    expect_is(bb, "list")
    expect_named(bb, c('langlinks', 'externallinks', 'common_names',
                       'classification', 'synonyms'))
    expect_is(bb$langlinks, "data.frame")
    expect_is(bb$externallinks, "character")
    expect_is(bb$common_names, "data.frame")
    expect_is(bb$classification, "data.frame")
    expect_named(bb$classification, c('rank', 'name'))
  })
})

test_that("wt_wikipedia fails well", {
  skip_on_cran()

  expect_error(wt_wikipedia(),
            "argument \"name\" is missing")
  expect_error(wt_wikipedia(5),
               "name must be of class character")

  # "name" must be length 1
  expect_error(
    wt_wikipedia(c("Pinus", "asdfadsf")),
    "length\\(name\\) == 1 is not TRUE"
  )

  # "wiki" must be character
  expect_error(
    wt_wikipedia("Pinus", 5),
    "wiki must be of class character"
  )

  # "utf8" must be logical
  expect_error(
    wt_wikipedia("Pinus", utf8 = "asdf"),
    "utf8 must be of class logical"
  )
})

context("wt_wikipedia_parse")

vcr::use_cassette("wt_wikipedia_parse", {
  test_that("wt_wikipedia_parse returns non-empty results", {
    skip_on_cran()

    url <- "https://species.wikimedia.org/wiki/Malus_domestica"
    pg <- wt_wiki_page(url)
    types <- c("common_names")
    result <- wt_wikipedia_parse(pg, types = types)
    expect_is(result, "list")
    for (fieldname in types) {
      expect_is(result[fieldname], "list")
      expect_gt(length(result[fieldname]), 0)
    }
  })
})

context("wt_wikipedia_search")

vcr::use_cassette("wt_wikipedia_search", {
  test_that("wt_wikipedia_search works", {
    skip_on_cran()

    aa <- wt_wikipedia_search(query = "Pinus")

    expect_is(aa, "list")
    expect_is(aa$continue, "list")
    expect_is(aa$query, "list")
    expect_is(aa$query$searchinfo, "list")
    expect_is(aa$query$search, "data.frame")
    expect_named(aa$query$search, c('ns', 'title', 'pageid', 'size', 'wordcount',
                                    'snippet', 'timestamp'))
  })
})


# no results when not found
vcr::use_cassette("wt_wikipedia_search_not_found", {
  test_that("wt_wikipedia_search_not_found", {
    expect_equal(NROW(wt_wikipedia_search("asdfadfaadfadfs")$query$search), 0)
  })
})

test_that("wt_wikipedia_search fails well", {
  skip_on_cran()

  expect_error(
    wt_wikipedia_search(),
    "argument \"query\" is missing"
  )
  expect_error(
    wt_wikipedia_search("Pinus", limit = "adf"),
    "limit must be of class integer, numeric"
  )
  expect_error(
    wt_wikipedia_search("Pinus", offset = "adf"),
    "offset must be of class integer, numeric"
  )
})
