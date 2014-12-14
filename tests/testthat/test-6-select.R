context("Test select()")

# Most of the underlying functionality has already been tested in interior
# functions, so this test suite primarily tests inputs and outputs and then
# runs the algorithm through a very constrained example.

test_that("Errors on invalid input parameters",{
  test_dat <- matrix(T,1,11)

  expect_error(select(),'argument "dat" is missing, with no default')

  # pop and dat have to be matrices
  expect_error(select(dat=""))

  # dat must have column names
  expect_error(select(dat=test_dat))
})

test_that("Output is properly formatted",{
  test_generations <- 15
  test_dat <- mtcars
  num_pred <-  ncol(test_dat)-1

  sel_result <- select(dat=test_dat,generations=test_generations)

  # Result should be a list with 5 elements
  expect_equal(class(sel_result),"ga_model")
  expect_equal(length(sel_result),5)

  # The final population is [num_preds x 2*num_preds]
  expect_equal(class(sel_result$pop),"matrix")
  expect_equal(dim(sel_result$pop),c(num_pred,2*num_pred))
  expect_equal(class(sel_result$pop[,1]),"logical")

  # The fitness matrix is [num_chromosomes x num_generations]
  # By default there are 10 generations
  expect_equal(class(sel_result$fit.val),"matrix")
  expect_equal(dim(sel_result$fit.val),c(2*num_pred,test_generations))
  expect_equal(class(sel_result$fit.val[,1]),"numeric")

  # The best chromosome should be same length as number of predictors
  expect_equal(class(sel_result$best_genes),"matrix")
  expect_equal(dim(sel_result$best_genes),c(num_pred,1))
  expect_equal(rownames(sel_result$best_genes),colnames(test_dat)[-1])

  # Default model is lm
  expect_equal(class(sel_result$model),"lm")

  # Fitness is a single number designating the fitness
  expect_equal(class(sel_result$fitness),"numeric")
  expect_equal(length(sel_result$fitness),1)
})
