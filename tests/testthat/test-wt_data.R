context("wt_data")

test_that("wt_data returns the correct class", {
  skip_on_cran()

  prop <- "P846"
  vcr::use_cassette("wt_data", {
    aa <- wt_data("Mimulus foliatus", property = prop)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(aa, "list")
  expect_is(aa$labels, "data.frame")
  expect_is(aa$descriptions, "data.frame")
  expect_is(aa$aliases, "data.frame")
  expect_is(aa$sitelinks, "data.frame")
  expect_is(aa$claims, "data.frame")
  expect_is(aa$claims, "data.frame")
  expect_equal(aa$claims$property, prop)
})

test_that("wt_data fails well", {
  expect_error(wt_data(), "argument \"x\" is missing, with no default")
})
