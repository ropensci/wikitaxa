context("wiki_data")

test_that("wiki_data returns the correct class", {
  prop <- "P846"
  aa <- wiki_data("Mimulus foliatus", property = prop)

  expect_is(aa, "list")
  expect_is(aa$labels, "data.frame")
  expect_is(aa$descriptions, "data.frame")
  expect_is(aa$aliases, "data.frame")
  expect_is(aa$sitelinks, "data.frame")
  expect_is(aa$claims, "data.frame")
  expect_is(aa$claims, "data.frame")
  expect_equal(aa$claims$property, prop)
})

test_that("wiki_data fails well", {
  expect_error(wiki_data(), "argument \"x\" is missing, with no default")
})
