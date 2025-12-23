irt_model_fit <- function(data_vector, nrow, ncol) {
  # Fit IRT model and return results as JSON 
  library("mirt")
  library("jsonlite")
  
  source("./irt_2pl.R")
  
  # Reshape data vector into matrix
  X <- matrix(data_vector, nrow = nrow, ncol = ncol, byrow = TRUE)
  colnames(X) <- paste0("Item", 1:ncol)
  
  # Fit and extract results
  res <- irt_2pl(X, verbose = TRUE)
  
  return(toJSON(list(difficulties = res$item_difficulty, abilities = res$ability)))
}
