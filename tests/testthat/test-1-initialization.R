context("Test init()")

test_that("Errors on incorrect chromosome length",{
  expect_error(init(),'argument "C" is missing, with no default')
  expect_error(init(-1),"invalid 'length' argument")
  expect_error(init("1"),'non-numeric argument to binary operator')
})

test_that("Chromosomes only contain logical values",{
  init_mat <- init(10)
  init_unique <- unique(sort(init_mat))

  expect_equal(class(init_mat),"matrix")
  expect_equal(class(init_mat[,1]),"logical")

  expect_equal(init_unique,c(F,T))
  expect_less_than(sum(init_mat),length(init_mat))
  expect_more_than(sum(init_mat),0)
})

test_that("Correct dimensions when C<25",{
  expect_null(dim(init(1)))
  expect_equal(length(init(1)),2)
  expect_equal(dim(init(10)),c(10,20))
  expect_equal(dim(init(25)),c(25,50))
})

test_that("Correct dimensions when C>25",{
  expect_equal(dim(init(26)),c(26,50))
  expect_equal(dim(init(50)),c(50,50))
  expect_equal(dim(init(100)),c(100,50))
})
