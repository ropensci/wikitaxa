context("wt_wiki_page/wt_wiki_page_parse")

test_that("wt_wiki_page returns a response object", {
  skip_on_cran()

  vcr::use_cassette("wt_wiki_page", {
    url <- "https://en.wikipedia.org/wiki/Malus_domestica"
    result <- wt_wiki_page(url)
  }, preserve_exact_body_bytes = TRUE)
  
  expect_is(result, "HttpResponse")
})

test_that("wt_wiki_page_parse returns non-empty results", {
  skip_on_cran()

  vcr::use_cassette("wt_wiki_page_non_empty", {
    url <- "https://en.wikipedia.org/wiki/Malus_domestica"
    pg <- wt_wiki_page(url)
    types <- c("langlinks", "iwlinks", "externallinks")
    result <- wt_wiki_page_parse(pg, types = types)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})

test_that("wt_wiki_page_parse returns non-empty results", {
  skip_on_cran()

  vcr::use_cassette("wt_wiki_page_non_empty_common_names", {
    url <- "https://en.wikipedia.org/wiki/Malus_domestica"
    pg <- wt_wiki_page(url)
    types <- c("common_names")
    result <- wt_wiki_page_parse(pg, types = types)
  }, preserve_exact_body_bytes = TRUE)
 
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})
