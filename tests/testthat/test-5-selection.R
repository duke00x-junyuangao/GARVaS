context("Test selection()")

# Register a parallel backend since selection() is parallelized
# across the pairs of parents in a population
doParallel::registerDoParallel(2)

test_that("Errors on invalid input parameters",{
  test_pop <- initialization(10)

  expect_error(selection(),'argument "pop" is missing, with no default')
  expect_error(selection(pop=test_pop),'argument "dat" is missing, with no default')
  expect_error(selection(pop=test_pop,dat=mtcars),'argument "f" is missing, with no default')
  expect_error(selection(pop=test_pop,dat=mtcars,f=AIC),'argument "model" is missing, with no default')

  # pop and dat have to be matrices
  expect_error(selection(pop="",dat="",f=AIC,model=lm))

  # dat must have column names
  test_dat <- matrix(T,1,11)
  expect_error(selection(pop=test_pop,dat=test_dat,f=AIC,model=lm))

  # pop must have same length as number of columns in dat
  test_pop <- initialization(9)
  expect_error(selection(pop=test_pop,dat=mtcars,f=AIC,model=lm))
})

test_that("Output is properly formatted",{
  test_pop <- initialization(ncol(mtcars)-1)
  sel_result <- selection(pop=test_pop,dat=mtcars,f=AIC,model=lm)

  # Result should be a list with 3 elements
  expect_equal(class(sel_result),"list")
  expect_equal(length(sel_result),3)

  # Parents should be same dimensions as pop
  expect_equal(dim(sel_result$parents),dim(test_pop))
  expect_equal(class(sel_result$parents[,1]),"logical")

  # Fittest should be a logical vector with right number of genes
  expect_equal(class(sel_result$fittest),"logical")
  expect_equal(length(sel_result$fittest),nrow(test_pop))

  # Fit should be a numeric vector with right number of chromosomes
  expect_equal(class(sel_result$fit),"numeric")
  expect_equal(length(sel_result$fit),ncol(test_pop))
})

test_that("Selection is reproduceable",{
  test_pop <- initialization(ncol(mtcars)-1)
  sel_result <- selection(pop=test_pop,dat=mtcars,f=AIC,model=lm)
  sel_result2 <- selection(pop=test_pop,dat=mtcars,f=AIC,model=lm)

  # The new parent population is stochastic but the fit and fittest
  # are deterministic and should be the same.
  expect_equal(sel_result$fittest,sel_result2$fittest)
  expect_equal(sel_result$fit,sel_result2$fit)
})

test_that("Results are representative of the fitness and model functions",{
  # Test population will test y~x1 and y~x2
  test_pop <- matrix(c(T,F,F,T),2,2)

  # Sample data with x1 linear with y and x2 shuffled
  test_dat <- data.frame(y=1:10,
                         x1=c(0.9,2.1,2.9,4.1,4.9,6.1,6.9,8.1,8.9,10.1),
                         x2=c(10,1,9,2,8,3,7,4,6,5))

  sel_result1 <- selection(pop=test_pop,dat=test_dat,f=AIC,model=lm)
  fit1_1 <- AIC(lm(y~x1,data=test_dat))
  fit1_2 <- AIC(lm(y~x2,data=test_dat))

  expect_equal(sel_result1$fittest,test_pop[,1])
  expect_equal(sel_result1$fit,c(fit1_1,fit1_2))

  # Very simple custom fit function
  test_fitness <- function(mod){
    summary(mod)$sigma
  }

  sel_result2 <- selection(pop=test_pop,dat=test_dat,f=test_fitness,model=lm)
  fit2_1 <- test_fitness(lm(y~x1,data=test_dat))
  fit2_2 <- test_fitness(lm(y~x2,data=test_dat))

  expect_equal(sel_result2$fittest,test_pop[,1])
  expect_equal(sel_result2$fit,c(fit2_1,fit2_2))

  # Contingency if all genes are turned off
  test_pop2<- matrix(c(F,F,F,F),2,2)
  sel_result3 <- selection(pop=test_pop2,dat=test_dat,f=AIC,model=lm)

  expect_equal(sel_result3$fittest,test_pop2[,1])
  expect_equal(sel_result3$fit,c(Inf,Inf))
})
