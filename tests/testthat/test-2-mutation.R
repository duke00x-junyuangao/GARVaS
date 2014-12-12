context("Test mutation()")

test_that("Errors on invalid chromosome input",{
  expect_error(mutation(),'argument "chr" is missing, with no default')
  expect_error(mutation(c("a","b","c","d")),
          "operations are possible only for numeric, logical or complex types")
})

test_that("Mutation does not change the length or type of the chromosome",{
  test_chr <- sample(c(T,F),100,replace=T)
  expect_equal(length(mutation(test_chr)),length(test_chr))
  expect_equal(class(mutation(test_chr)),class(test_chr))
})

test_that("Mutations do not happen too often or too rarely",{
  test_mat <- replicate(100,sample(c(T,F),100,replace=T))
  mut_mat <- apply(test_mat,2,mutation)
  num_muts <- sum(xor(test_mat,mut_mat))

  # Since mutation rate is 1%, we should get between .5% and 1.5% mutations
  expect_less_than(num_muts, 150)
  expect_more_than(num_muts,50)
})
