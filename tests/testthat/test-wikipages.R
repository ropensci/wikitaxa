context("wikipages")

test_that("wt_wiki_url_parse correctly parses static page url", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  result <- wt_wiki_url_parse(url)
  expect_is(result, "list")
  expect_equal(result$wiki, "en")
  expect_equal(result$type, "wikipedia")
  expect_equal(result$page, "Malus_domestica")
})

test_that("wt_wiki_url_parse correctly parses API page url", {
  url <- "https://en.wikipedia.org/w/api.php?page=Malus_domestica"
  result <- wt_wiki_url_parse(url)
  expect_is(result, "list")
  expect_equal(result$wiki, "en")
  expect_equal(result$type, "wikipedia")
  expect_equal(result$page, "Malus_domestica")
})

test_that("wt_wiki_url_build correctly builds static page url", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  result <- wt_wiki_url_build("en", "wikipedia", "Malus domestica")
  expect_equal(result, url)
})

test_that("wt_wiki_url_build correctly builds API page url", {
  url <- "https://en.wikipedia.org/w/api.php?page=Malus_domestica&action=parse&redirects=TRUE&format=json&utf8=TRUE&prop=text"
  result <- wt_wiki_url_build("en", "wikipedia", "Malus domestica", api = TRUE, action = "parse", redirects = TRUE, format = "json", utf8 = TRUE, prop = "text")
  expect_equal(result, url)
})

test_that("wt_wiki_page returns a response object", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  result <- wt_wiki_page(url)
  expect_is(result, "response")
})

test_that("wt_wiki_page_parse returns non-empty results", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  pg <- wt_wiki_page(url)
  types <- c("langlinks", "iwlinks", "externallinks")
  result <- wt_wiki_page_parse(pg, types = types)
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})

test_that("wt_wiki_page_parse returns non-empty results", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  pg <- wt_wiki_page(url)
  types <- c("common_names")
  result <- wt_wiki_page_parse(pg, types = types)
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})

test_that("wt_wikicommons_parse returns non-empty results", {
  url <- "https://commons.wikimedia.org/wiki/Malus_domestica"
  pg <- wt_wiki_page(url)
  types <- c("common_names")
  result <- wt_wikicommons_parse(pg, types = types)
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})

test_that("wt_wikispecies_parse returns non-empty results", {
  url <- "https://species.wikimedia.org/wiki/Malus_domestica"
  pg <- wt_wiki_page(url)
  types <- c("common_names")
  result <- wt_wikispecies_parse(pg, types = types)
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})
