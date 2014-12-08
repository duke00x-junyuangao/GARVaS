context("Test genetic algorithm")

test_that("Test the 1st round initialization",{
  expect_equal(dim(init(10)), c(10,30))
})
