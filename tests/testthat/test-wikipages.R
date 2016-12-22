context("wikipages")

test_that("parse_wiki_url correctly parses static page url", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  result <- parse_wiki_url(url)
  expect_is(result, "list")
  expect_equal(result$wiki, "en")
  expect_equal(result$type, "wikipedia")
  expect_equal(result$page, "Malus_domestica")
})

test_that("parse_wiki_url correctly parses API page url", {
  url <- "https://en.wikipedia.org/w/api.php?page=Malus_domestica"
  result <- parse_wiki_url(url)
  expect_is(result, "list")
  expect_equal(result$wiki, "en")
  expect_equal(result$type, "wikipedia")
  expect_equal(result$page, "Malus_domestica")
})

test_that("build_wiki_url correctly builds static page url", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  result <- build_wiki_url("en", "wikipedia", "Malus domestica")
  expect_equal(result, url)
})

test_that("build_wiki_url correctly builds API page url", {
  url <- "https://en.wikipedia.org/w/api.php?page=Malus_domestica&action=parse&redirects=TRUE&format=json&utf8=TRUE&prop=text"
  result <- build_wiki_url("en", "wikipedia", "Malus domestica", api = TRUE, action = "parse", redirects = TRUE, format = "json", utf8 = TRUE, prop = "text")
  expect_equal(result, url)
})

test_that("get_wiki_page returns a response object", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  result <- get_wiki_page(url)
  expect_is(result, "response")
})

test_that("parse_wiki_page returns non-empty results", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  pg <- get_wiki_page(url)
  types <- c("langlinks", "iwlinks", "externallinks")
  result <- parse_wiki_page(pg, types = types)
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})

test_that("parse_wikipedia_page returns non-empty results", {
  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  pg <- get_wiki_page(url)
  types <- c("common_names")
  result <- parse_wikipedia_page(pg, types = types)
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})

test_that("parse_wikicommons_page returns non-empty results", {
  url <- "https://commons.wikimedia.org/wiki/Malus_domestica"
  pg <- get_wiki_page(url)
  types <- c("common_names")
  result <- parse_wikicommons_page(pg, types = types)
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})

test_that("parse_wikispecies_page returns non-empty results", {
  url <- "https://species.wikimedia.org/wiki/Malus_domestica"
  pg <- get_wiki_page(url)
  types <- c("common_names")
  result <- parse_wikispecies_page(pg, types = types)
  expect_is(result, "list")
  for (fieldname in types) {
    expect_is(result[fieldname], "list")
    expect_gt(length(result[fieldname]), 0)
  }
})
