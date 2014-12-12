context("Test production()")

# Register a parallel backend since production() is parallelized
# across the pairs of parents in a population
doParallel::registerDoParallel(2)

test_that("Errors on invalid parent population",{
  expect_error(production(),'argument "pop" is missing, with no default')
  expect_error(production(""),'"matrix" is not TRUE')
  expect_error(production(matrix(1,4,3)),'%%2 is not TRUE')
})

test_that("The new population has the same type and dimensions as input",{
  init_pop <- initialization(10)
  new_pop <- production(init_pop)

  expect_equal(class(init_pop),class(new_pop))
  expect_equal(class(init_pop[,1]),class(new_pop[,1]))
  expect_equal(dim(init_pop),dim(new_pop))
})

test_that("Production permutes and mutates the parent population",{
  init_pop <- initialization(10)
  new_pop1 <- production(init_pop)
  new_pop2 <- production(init_pop)
  new_pop3 <- production(new_pop1)

  # These are not very indicative tests of what production does
  # beyond that it just does something to permute the population
  # Unfortunately stochasticity means it's very hard to test correct behavior

  expect_false(identical(init_pop,new_pop1))
  expect_false(identical(init_pop,new_pop2))
  expect_false(identical(init_pop,new_pop3))
  expect_false(identical(new_pop1,new_pop2))
  expect_false(identical(new_pop1,new_pop3))
  expect_false(identical(new_pop2,new_pop3))
})
