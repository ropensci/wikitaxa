context("wt_wiki_url_parse")

test_that("wt_wiki_url_parse correctly parses static page url", {
  skip_on_cran()

  url <- "https://en.wikipedia.org/wiki/Malus_domestica"
  result <- wt_wiki_url_parse(url)
  expect_is(result, "list")
  expect_equal(result$wiki, "en")
  expect_equal(result$type, "wikipedia")
  expect_equal(result$page, "Malus_domestica")
})

test_that("wt_wiki_url_parse correctly parses API page url", {
  skip_on_cran()

  url <- "https://en.wikipedia.org/w/api.php?page=Malus_domestica"
  result <- wt_wiki_url_parse(url)
  expect_is(result, "list")
  expect_equal(result$wiki, "en")
  expect_equal(result$type, "wikipedia")
  expect_equal(result$page, "Malus_domestica")
})
