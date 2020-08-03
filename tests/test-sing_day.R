context("sing day")
test_that("Throw an error ", {

  expect_error(sing_day(dne, 1, "Rando"))

})
