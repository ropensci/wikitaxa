context("wt_wiki_url_build")

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
