
rasch_model_fit <- function(data_vector, nrow, ncol) {
  # Fit Rasch model and return results as JSON 
  library("eRm")
  library("jsonlite")
  
  source("./rasch_erm.R")
  
  # Reshape data vector into matrix
  X <- matrix(data_vector, nrow = nrow, ncol = ncol, byrow = TRUE)
  
  # Fit and extract results
  res <- rasch_erm(X, verbose = TRUE)
  
  return(toJSON(list(difficulties = res$item_difficulty, abilities = res$ability)))
}

rasch_model_fit_withRandData <- function(numItem, numPerson) {
  # Fit Rasch model and return results as JSON 
  library("eRm")
  library("jsonlite")
  
  source("./rasch_erm.R")
  
  # Example data: simulate a small Rasch dataset
  set.seed(42)
  N <- numPerson
  J <- numItem
  theta <- rnorm(N, 0, 1)
  b <- rnorm(J, 0, 0.7); b <- b - mean(b)
  P <- plogis(outer(theta, -b, "+"))
  X <- matrix(rbinom(N * J, 1, as.vector(P)), nrow = N, ncol = J)
  
  # Fit and extract results
  res <- rasch_erm(X, verbose = TRUE)
  
  return(toJSON(list(difficulties = res$item_difficulty, abilities = res$ability)))
}