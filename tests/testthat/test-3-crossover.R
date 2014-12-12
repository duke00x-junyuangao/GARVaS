context("Test crossover()")

test_that("Errors on invalid chromosome input",{
  expect_error(crossover(),'argument "chr1" is missing, with no default')
  expect_error(crossover(T),'argument "chr2" is missing, with no default')
  expect_error(crossover(c(T),c(T,F)),'C1 == C2 is not TRUE')
})

test_that("Crossover does not change the input type or length",{
  chr1 <- sample(c(T,F),10,replace=T)
  chr2 <- sample(c(T,F),10,replace=T)

  cross <- crossover(chr1,chr2)

  expect_equal(dim(cross),c(10,2))
  expect_equal(class(cross),"matrix")

  expect_equal(length(chr1),length(cross[,1]))
  expect_equal(class(chr1),class(cross[,1]))

  expect_equal(length(chr2),length(cross[,2]))
  expect_equal(class(chr2),class(cross[,2]))
})

test_that("Crossed chromosomes are not identical to originals",{

  chr1 <- rep(T,100)
  chr2 <- rep(F,100)

  cross <- crossover(chr1,chr2)

  num_same_1 <- sum(xor(chr1,cross[,1]))
  num_same_2 <- sum(xor(chr2,cross[,2]))

  # Since the two vectors are completely opposite, if crossover happened
  # the number of identical elements should be less than the original length
  expect_less_than(num_same_1,100)
  expect_less_than(num_same_2,100)
})

test_that("Repeated crossover is random",{
  chr1 <- rep(T,100)
  chr2 <- rep(F,100)

  cross1 <- crossover(chr1,chr2)
  cross2 <- crossover(chr1,chr2)
  cross3 <- crossover(cross1[,1],cross1[,2])

  expect_false(identical(cross1[,1],cross2[,1]))
  expect_false(identical(cross1[,2],cross2[,2]))

  # Make sure cross-over is not undoing any crossing
  expect_false(identical(chr1,cross3[,1]))
  expect_false(identical(chr2,cross3[,2]))
})

test_that("Crossover only happens at a single point",{
  chr1 <- rep(T,100)
  chr2 <- rep(F,100)

  cross <- crossover(chr1,chr2)

  # Mutations will happen, but since the mutation breaks a contiguous sequence
  # of T or F, the diff count will cancel out. Thus sum of diff will be odd if
  # a single crossover takes place between all-T and all-F chromosomes

  # A single flip from T to F; F-T = -1
  expect_equal(sum(diff(cross[,1])),-1)

  # A single flip from F to T; T-F = 1
  expect_equal(sum(diff(cross[,2])),1)
})
