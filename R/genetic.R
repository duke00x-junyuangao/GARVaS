#' Genetic Algorithm for Variable Selection
#'
#' This function selects best model for users based on genetic algorithm.
#'
#' @usage
#' select(dat,generations=10,f=AIC,model="lm")
#' select(dat,generations=10,f=BIC,model="glm")
#'
#' @param dat data frame containing the predictors in the model.
#' First column should be the response variable.
#'
#' @param generations number of generations users want to generate.
#' If the f argument is missing, the default is 10.
#'
#' @param f function that is defined by users for calculating fitting scores.
#' Users can choose AIC or BIC or even define by themselves.
#' If the f argument is missing, the default is AIC.
#'
#' @param model the linear model that user wants to use to fit in the data.
#' It can be either \code{lm} or \code{glm}, with the default being \code{lm}.
#'
#' @param ... additional arguments to pass to the model.
#'
#' @details This function will give users best linear regression model selected
#' by genetic algorithm. It allows users to choose when to stop the algorithm
#' and what kind of criteria they want to implement in the algorithm.
#'
#' @section Parallel Computation: The internal fitness calculations use parallel
#' for-loop to rank the chromosomes. By default the parallel back-end will scale
#' to use all cores on the machine of choice.
#'
#' @return It returns a list containg the last generation chromosomes matrix,
#' the fittest chromosomes, the fitting scores of each chromosome in the last
#' generation and the best model selected.
#'
#' @examples
#' dat<-mtcars
#' select(dat)
#' select(dat,generations=25,f=AIC,model=lm)
#' select(dat,generations=25,f=BIC,model=glm,family=gaussian)
#'
#' @export
select <- function(dat, generations=10, f=AIC, model=lm, ...){

  # Input dataset must have column names
  stopifnot(!is.null(colnames(dat)))

  # Load the doParallel and foreach packages if not yet loaded
  if(!requireNamespace("doParallel", quietly = TRUE)|
     !requireNamespace("foreach", quietly = TRUE)){
    stop("GARVaS requires doParallel and foreach for parallel computation")
  }

  # Extract predictor and response names
  response <- colnames(dat)[1]
  predictors <- colnames(dat)[-1]

  # Register the parallel cluster for subsequent computation
  nCores <- parallel::detectCores()
  doParallel::registerDoParallel(nCores)

  # Randomize the initial chromosomes
  pop <- initialization(ncol(dat) - 1)

  # Placeholder matrices
  fit.val <- matrix(0,ncol(pop), generations)

  # Run the first round of selection
  sel.result <- selection(pop,f,dat,model,...)

  # Perform the speficied number of generations
  for (i in 1:generations){
    fit.val[,i] <- sel.result$fit
    pop <- production(sel.result$parents)
    sel.result <- selection(pop,f,dat,model,...)
  }

  # Extract and label the fittest chromosome
  fittest <- matrix(sel.result$fittest)
  rownames(fittest) <- predictors

  # Get the final model formula
  form <- as.formula(paste(response, "~", paste(predictors[sel.result$fittest], collapse = "+")))
  mod <- model(form, data=dat, ...)

  # Use an S3 class for GA selection
  GA_model <- list()
  class(GA_model) <- "ga_model"

  # Return four things:
  # 1) A matrix of the last generation.
  # 2) The fittest individual in the last generation.
  # 3) The fitness score of the over generation for each model.
  # 4) linear model using the chosen variables.
  # 5) The fitness score for the selected model.
  GA_model$pop <- sel.result$parents
  GA_model$best_genes <- fittest
  GA_model$fit.val <- fit.val
  GA_model$model <- mod
  GA_model$fitness <- f(mod)

  # Return the new GA model
  GA_model
}

# Use S3 method to print out the fitted score
# for the chosen model and the chosen model itself.
#' @export
print.ga_model <- function(ga_model,...){
  cat("Final Model:\n")
  print(ga_model$mod$coefficients)
  cat("\n")
  cat(paste("Fitness Score: ",ga_model$fitness,"\n"))
}

# Use S3 method to plot boxplot for fitness score for each generataion.
# As we can see in plot, the fitness score converges as generation expands.
#' @export
plot.ga_model <- function(ga_model,...){
  boxplot(ga_model$fit.val, xlab="Generation", ylab="Fitness Score",
          main="Distribution of Fitness Scores Across Generations")
}

#' Manual Testing of Underlying GARVaS Functions
#'
#' Normally the package is tested automatically by running
#' \code{devtools::test()} but for simplicity the \code{test()} function
#' will run all unit tests when sourced and executed.
#'
#' Functionality is provided by \code{testthat} package so it needs to
#' be installed in order for the tests to execute.
#'
#' @export
test <- function(){
  testthat::test_dir("tests/testthat")
}

#' Generate The 1st Generation of the Genetic Algorithm
#'
#' Bernoulli sampling to form the first generation of the algorithm.
#' Since interactions are possible, 2*C is a valid population size but
#' imposing an upper limit of 50 doesn't hurt.
#'
#' @usage initialization(C)
#'
#' @param C chromosome length (Number of Variables in the data)
#'
#' @details This function produces first generation given the chromosome length
#' for Genetic Algorithm. The row of the generated boolean matrix represents the
#' locus of a gene(variable), and the column of the matrix represents different
#' members of the first generation(different models). The number of first
#' generation is defined by minimum(2C,50).
#'
#' @return Boleans Matrix, [C x 2C] or [C x 50], with each column
#' representing a chromosome, where T marks that the gene (variable)
#' as active and F as inactive.
#'
#' @examples
#' initialization(10)
#' initialization(20)
initialization <- function(C){
  P <- min(2*C, 50)
  replicate(P, sample(c(F,T), size=C, replace=T))
}

#' Chromosome Mutation
#'
#' Make a chromosome with fixed probability 0.01 to mutate
#'
#' @usage mutation(chr)
#'
#' @param chr a logical vector representing an individual chromosome.
#'
#' @details This function makes a chromosome have a 1% fixed chance to mutate
#' in each locale. If mutation happens at one locale, it will make the value
#' in that locale from T to F(or F to T).
#'
#' @return Return a mutated chromosome vector
#' with the same length as input one.
#'
#' @examples
#' ind<-initialization(9)[,1]
#' mutation(ind)
mutation <- function(chr){

  # For each element of chr determine whether to mutate with 1% prob
  do_change <- sample(c(T,F),length(chr),prob=c(0.01,0.99),replace=T)

  # 'exclusive or' will toggle F to T and T to F
  xor(chr,do_change)
}

#' Chromosome Crossover and Mutation
#'
#' Make 2 individual parent chromosomes crossover and
#' mutate when breeding offsprings.
#'
#' @usage crossover(chr1,chr2)
#'
#' @param chr1,chr2 a numeric vectors represents individual parent chromosome.
#'
#' @details This function makes two individual parent chromosomes
#' perfrom crossover and mutation when breeding next generation.
#' Note that the crossover is simply one-point crossover
#' and the mutation is based on \code{GARVaS::mutation()}.
#'
#' @return A matrix with each column representing the
#' offsprings from a process of crossover and mutation.
#'
#' @examples
#' ind1<-initialization(9)[,1]
#' ind2<-initialization(9)[,2]
#' crossover(ind1,ind2)
crossover <- function(chr1, chr2){

  C1 <- length(chr1)
  C2 <- length(chr2)

  # Make sure we're working with equal-sized vectors
  stopifnot(C1==C2)

  #randomly select a point as the point of crossover.
  k <- sample(1:(C1-1), 1)

  # Return the crossed chromosomes together
  cbind(
    mutation(c(chr1[1:k], chr2[(k+1):C1])),
    mutation(c(chr2[1:k], chr1[(k+1):C1]))
  )
}

#' Produce Next Generation
#'
#' Breeding next generation from selected parent chromosomes.
#'
#' @usage production(pop)
#'
#' @param pop Selected parent chromosome matrix from \code{GARVaS::selection()}
#'
#' @details This function produces the next generation by randomly choosing
#' 2 parent chromosomes from selected parent matrix produced to perform the
#' process of crossover and mutation using \code{GARVaS::crossover()}
#'
#' @return It returns next generation matrix with each
#' column representing new generation chromosome.
#'
#' @examples
#' dat<-mtcars #Get ozone data from faraway package
#' C<-dim(dat)[2]-1 #Number of variables
#' pop<-initialization(C) #produce boleans matrix
#' production(pop) #Produce next generation.
production <- function(pop){

  # Population must be a matrix with even number of columns
  stopifnot(class(pop)=='matrix')
  stopifnot(!ncol(pop)%%2)

  # Randomize the parent assignment
  P <- ncol(pop)
  pop <- pop[,sample(1:P)]

  # Cross-over pairs of parents in parallel
  foreach::`%dopar%`(foreach::foreach(i = 1:(P/2), .combine=cbind), {
    crossover(pop[,i], pop[,i+P/2])
  })
}

#' Select Parents Generated From \code{GARVaS::initialization()}
#' Based on Given Fitness Function
#'
#' Selecting parents based on fitness ranks, with AIC as the
#' default fitness criteria. Alternatively, we can use tournament selection.
#'
#' @usage
#' selection(pop,f=AIC,dat)
#' selection(pop,f=BIC,dat)
#'
#' @param pop boleans matrix determined by \code{GARVaS::initialization()}
#'
#' @param f fitness function that takes in an lm or glm model and returns a
#' a numerical 'qualification' of that model. Users can choose AIC or BIC or
#' even define by themselves. If the f argument is missing, the default is AIC.
#'
#' @param dat data frame containing the predictors in the model.
#' First column should be the response variable.
#'
#' @param model the linear model that user wants to use to fit in the data,
#' can be either \code{lm} or \code{glm}.
#'
#' @param ... additional arguments to pass to the model function.
#'
#' @details This function selects parents to breed by fitting linear regression
#' model to each possible parent generated by \code{GARVaS::initialization()}
#' based on fitness rank with AIC as the default fitness function.
#'
#' @return Return a list, where containing chosen parents, fittest parent
#' and fitting score for each parent based on fitting function.
#'
#' @examples
#' dat<-mtcars #Get ozone data from faraway package
#' C<-dim(dat)[2]-1 #Number of variables
#' pop<-initialization(C) #produce boleans matrix
#' selection(pop,f=AIC,dat)
selection <- function(pop, f, dat, model, ...){

  # number of chromosomes
  P <- ncol(pop)

  # Variables are columns of the dataset
  response <- colnames(dat)[1]
  predictors <- colnames(dat)[-1]

  # Input parameters must be named matrices
  stopifnot(!is.null(P) && !is.null(response) && !is.null(predictors))

  # Number of genes must equal number of predictors
  stopifnot(nrow(pop)==length(predictors))

  # Process each chromosome in parallel
  fit <- foreach::`%dopar%`(foreach::foreach(i = 1:P, .combine=c),{

    # Build a formula using the chosen predictors
    chosen <- pop[,i]

    # Account for situation when all predictors are F
    if(sum(chosen)){
      form <- as.formula(paste(response, "~",
                               paste(predictors[chosen], collapse = "+")))

      # Calculate the fitness using the provided fitness function
      f(model(formula=form,data=dat,...))
    }else{
      Inf
    }
  })

  # Compute a vector of probability weights
  # Since lowest fitness is the best, take the reverse rank
  fitness <- 2*rank(-fit)/(P*(P+1))

  # Sample from the P chromosomes, with weights specified in fitness,
  # with replacement, to generate a parenting population of size P.
  # Note there are duplicates within the parenting population.
  parent_ind <- sample(x=1:P, size=P, replace=T, prob=fitness)
  parents <- pop[,parent_ind]

  # Keep a copy of the fittest individual.
  fittest <- pop[,which.max(fitness)]

  # Build the selection result as a list
  sel.result <- list()
  sel.result$parents <- parents
  sel.result$fittest <- fittest
  sel.result$fit <- fit

  # Return the result
  sel.result
}
