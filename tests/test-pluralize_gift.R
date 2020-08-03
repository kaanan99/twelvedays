context("pluralize gift")
test_that("Special cases will be pluralized", {

  test_list <- c("car", "family", "foot")
  answer_list <- c("car", "families", "feet")
  result <- pluralize_gift(test_list)
  expect_equivalent(result, answer_list)

})
