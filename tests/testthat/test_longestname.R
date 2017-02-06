context("Longest Name")

test_that("longestname outputs a name", {
  expect_that(longestname(2015), is_a("character"))
})
