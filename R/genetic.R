#' Genetic Algorithm for Variable Selection
#'
#' This function selects best model for users based on genetic algorithm.
#'
#' @usage select(dat,generations=10,f=AIC,model="lm",graph=TRUE)
#' select(dat,generations=10,f=BIC,model="glm",graph=TRUE)
#' @param dat data frame containing the predictors in the model.
#' First column should be the response variable.
#' @param generations number of generations users want to generate.
#' If the f argument is missing, the default is 10.
#' @param f function that is defined by users for calculating fitting scores.
#' Users can choose AIC or BIC or even define by themselves.
#' If the f argument is missing, the default is AIC.
#' @param model the linear model that user wants to use to fit in the data.
#' It can be either "lm" or "glm", with the default being "lm".
#' @param family the family argument in the glm() function, if model was selected as "glm".
#' @param graph if graph=TRUE, the function will plot the convergence plot based on fitting score.
#' If the f argument is missing, the default is TRUE.
#' @details This function will give users best linear regression model selected by genetic algorithm.
#' It allows users to choose when to stop the algorithm and what kind of criteria they want to implement in
#' the algorithm. And the convergence graph can also let users know when to stop the algorithm.
#' @section Parallel Computation: The internal fitness calculations use parallel for-loop to rank the chromosomes.
#' By default the parallel back-end will scale to use all cores on the machine of choice.
#' @return It returns a list containg the last generation chromosomes matrix, the fittest chromosomes,
#' the fitting scores of each chromosome in the last generation and the best model selected.
#' @examples
#' library(faraway)
#' dat=ozone #Get ozone data from faraway package
#' select(dat)
#' select(dat,generations=25,f=AIC, graph=FALSE)
#' @export
select <- function(dat, generations=10, f=AIC, model="lm", family=gaussian, graph=TRUE){

  # Make sure the parallel packages are installed and available
  if(! requireNamespace("doParallel", quietly = TRUE) |
       !requireNamespace("foreach", quietly = TRUE)){
    stop("GARVaS requires doParallel and foreach for parallel computation")
  }

  # Register the parallel cluster for subsequent computation
  nCores <- parallel::detectCores()
  doParallel::registerDoParallel(nCores)

  # Exclude the 1st column (the response) from the population of predictors
  C <- ncol(dat) - 1
  pop <- init(C)

  # Placeholder matrices
  fit.val <- matrix(0,min(2*C, 50),generations)
  generation.mat <- matrix(rep(1:generations,each=min(2*C),50),min((2*C),50),generations)

  # Perform the speficied number of generations
  for (i in 1:generations){
    sel.result <- selection(pop,f,dat,model,family)
    parents <- sel.result[[1]]
    fit.val[,i] <- sel.result[[3]]
    pop <- produce(parents)
  }

  # Plot the results if desired
  if (graph){
    matplot(generation.mat,fit.val,type="p",pch=4,col=1,xlab="Generation",ylab="Fitted Value")
  }

  # Calculate the final fitness of the selected population
  fittest <- selection(pop,f,dat,model,family)[[2]]

  # Get the final model formula
  cols <- colnames(dat)
  chosen <- which(fittest[,1] == 1) + 1
  form <- as.formula(paste(cols[1], "~", paste(cols[chosen], collapse = "+")))
  if (model=="lm"){mod <- lm(form, data=dat)}
  else{mod <- glm(form, data=dat, family=family)}

  #Return four things: a matrix of the last generation; a listing of the fittest individual(s);
  #The AIC score of the last generation; and a lm() model using the chosen variables.
  #After 10 generations, the fittest ones are mostly the same, which shows convergence.
  list(pop, fittest, fit.val, mod)
}

#' Generate The 1st Generation of the Genetic Algorithm
#'
#' Bernoulli sampling to form the first generation of the algorithm.
#' Since interactions are possible, 2*C is a valid population size but
#' imposing an upper limit of 50 doesn't hurt.
#'
#' @usage init(C)
#' @param C chromosome length (Number of Variables in the data)
#' @details This function produces first generation given the chromosome length for Genetic Algorithm. The row of
#' generated booleans matrix represents locus of a gene(variable), and the column of the matrix represents different member
#' of the first generation(different models). The number of first generation is defined by minimum(2C,50).
#' @return Boleans Matrix, [C x 2C] or [C x 50], with each column representing a chromosome,
#'  where 1 represent the selecting the gene(variable), while zero not.
#' @examples
#' init(C=10)
#' init(C=20)
init <- function(C){
  P <- min(2*C, 50)
  replicate(P, sample(c(0,1), size=C, replace=T))
}

#' Select Parents Generated From \code{GARVaS::init()} Based on given Criteria.
#'
#' Selecting parents based on fitness ranks, with AIC as the default fitness criteria.
#' Alternatively, we can use tournament selection.
#'
#' @usage selection(pop,f=AIC,dat)
#' selection(pop,f=BIC,dat)
#' @param pop boleans matrix determined by \code{GARVaS::init()}
#' @param f fitness function that takes in an lm or glm model and returns a
#' a numerical 'qualification' of that model. Users can choose AIC or BIC or even define by themselves.
#' If the f argument is missing, the default is AIC.
#' @param dat data frame containing the predictors in the model.
#' First column should be the response variable.
#' @param model the linear model that user wants to use to fit in the data, can be either "lm" or "glm".
#' @param family the family argument in the glm() function, if model was selected as "glm".
#' @details This function selects parents to breed by fitting linear regression model
#' to each possible parent generated by \code{GARVaS::init()} based on fitness rank
#' with AIC as the default fitness function.
#' @return Return a list, where containing chosen parents, fittest parent
#' and fitting score for each parent based on fitting function.
#' @examples
#' library(faraway)
#' dat=ozone #Get ozone data from faraway package
#' C=dim(dat)[2]-1 #Number of variables
#' pop=init(C) #produce boleans matrix
#' selection(pop,f=AIC,dat)
selection <- function(pop, f=AIC, dat, model="lm", family=gaussian){

  # number of individuals
  P <- ncol(pop)

  # Variables are columns of the dataset
  cols <- colnames(dat)

  # Use parallel:
  fit <- foreach::foreach(i = 1:P, .combine=c) %dopar% {

    # Find the chosen predictors and use them as index
    chosen <- which(pop[,i]==1) + 1

    # Build a formula using the chosen predictors
    form <- as.formula(paste(cols[1], "~", paste(cols[chosen], collapse = "+")))

    # Built the linear model
    if (model=="lm"){mod <- lm(form, data=dat)}
    else{mod <- glm(form, data=dat, family=family)}

    # Calculate the fitness using the provided fitness function
    # Lowest AIC has the highest rank so take the negative
    -f(mod)
  }

  # Compute a vector of probability weights
  fitness <- 2*rank(fit)/(P*(P+1))

  # Sample from the P individuals, with weights specified as in fitness,
  # with replacement, to generate a parenting population of size P.
  # Note there are duplicates within the parenting population.
  parent_ind <- sample(x=1:P, size=P, replace=T, prob=fitness)
  parents <- pop[ ,parent_ind]

  # Keep a copy of the fittest individual.
  fittest <- pop[,which(fitness==max(fitness))]

  # Return the results as a list
  list(parents, fittest, -fit)
}

#' Chromosome Mutation
#'
#' Make a chromosome with fixed probability 0.01 to mute
#'
#' @usage mutation(chr)
#' @param chr a numeric vector represent a individual chromosome.
#' @details This function makes a chromosome has a 1% fixed chance to mute in each locale. If mutation
#' happens at one locale, it will make the value in that locale from 1 to 0(or 0 to 1).
#' @return It returns a mutated chromosone vector with the same length as input one.
#' @examples
#' ind=init(9)[,1] #Generate a chromosome
#' mutation(ind)
mutation <- function(chr){
  for (i in 1:length(chr)){
    a <- sample(1:100, size=1)

    #If mutation happens at one locale, change the 1 at that postition to 0(or 0 to 1).
    if (a==1){chr[i] = 1 - chr[i]}
    else{next}
  }

  # Return the mutated chromosome
  chr
}

#' Chromosome Crossover and Mutation
#'
#' Make 2 individual parent chromosomes crossover and mute when breeding offsprings.
#'
#' @usage crossover(chr1,chr2)
#' @param chr1,chr2 a numeric vectors represents individual parent chromosome.
#' @details This function makes two individual parent chromosomes perfrom crossover and mutation
#' when breeding next generation. Note that the crossover is simply one-point crossover
#' and the mutation is based on \code{GARVaS::mutation()}.
#' @return A matrix with each column representing the
#' offsprings from a process of crossover and mutation.
#' @examples
#' ind1=init(9)[,1]
#' ind2=init(9)[,2]
#' crossover(ind1,ind2)
crossover <- function(chr1, chr2){
  C <- length(chr1)

  #randomly select a point as the point of crossover.
  k <- sample(1:(C-1), 1)
  chr1_new <- mutation(c(chr1[1:k], chr2[(k+1):C]))
  chr2_new <- mutation(c(chr2[1:k], chr1[(k+1):C]))

  # Return the joined chromosomes
  cbind(chr1_new, chr2_new)
}

#' Produce Next Generation
#'
#' Breeding next generation from selected parent chromosomes.
#'
#' @usage produce(pop)
#' @param pop Selected parent chromosomes matrix from \code{GARVaS::selection()}
#' @details This function produces the next generation by randomly choosing 2 parent
#' chromosomes from selected parent matrix produced by \code{GARVaS::selection()}
#' to perform the process of crossover and mutation which is based on the \code{GARVaS::crossover()}
#' @return It returns next generation matrix with each column representing
#' new generation chromosome.
#' @examples
#' library(faraway)
#' dat=ozone #Get ozone data from faraway package
#' C=dim(dat)[2]-1 #Number of variables
#' pop=init(C) #produce boleans matrix
#' pop=selection(pop,f=AIC,dat) #Generare a selected parent matrix
#' produce(pop) #Produce next generation.
produce <- function(pop){
  P <- ncol(pop)

  #Randomize the parent assignment
  order <- sample(1:P, size=P)

  #The first two offspring generated by the first pair of parents.
  newGen <- crossover(pop[,order[1]], pop[,order[2]])
  for (i in 2:(P/2)){
    newGen <- cbind(newGen, crossover(pop[,order[2*i-1]], pop[,order[2*i]]))
  }

  # Return the new generation
  newGen
}
