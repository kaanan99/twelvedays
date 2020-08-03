context("make phrase")
test_that("Test make phrase with all attributes", {

  result <- make_phrase(5, "fifth", "clocks", "ticking", "big", "in the living room")
  answer <- "five big clocks ticking in the living room"
  expect_equivalent(result, answer)

})
