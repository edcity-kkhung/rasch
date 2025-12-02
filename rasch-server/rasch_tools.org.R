library("eRm")
library("jsonlite")

get_rank <- function(score, sorted_theta) {
  # Check edge cases
  if (score > max(sorted_theta)) {
    return(1)  # 1st rank
  } else if (score < min(sorted_theta)) {
    return(length(sorted_theta) + 1)  # Last rank
  } else {
    # Find first position where theta <= score
    return(which.max(sorted_theta <= score))
  }
}

calc_theta_refit <- function(model, response) {
  # # Validate model type
  # if (!inherits(model, "RM")) {
  #   stop("Model must be an object of class 'RM' from eRm::RM().")
  # }
  
  # Convert vector to data frame if needed
  new_response <- response
  if (!is.data.frame(new_response)) {
    new_response <- as.data.frame(t(new_response))
  }
  
  # Ensure correct number of items
  if (ncol(new_response) != ncol(model$X)) {
    stop("Number of items in new_response does not match the model.")
  }
  
  # ✅ Fix column names to match original data
  colnames(new_response) <- colnames(model$X)
  
  # Combine original data with new response
  extended_data <- rbind(model$X, new_response)

  # Refit the Rasch model
  refitted_model <- RM(extended_data)

  # Extract person parameters for new responses
  pp <- person.parameter(refitted_model)

  # Get theta for the new response (last row)
  new_theta <- pp$theta.table[nrow(extended_data), "Person Parameter"]

  # Calculate new rank
  new_rank <- get_rank(new_theta, sort(pp$theta.table$`Person Parameter`, decreasing = TRUE))

  # return(list(response = toJSON(response), theta = new_theta, rank = new_rank, rankTotal = nrow(extended_data), model = refitted_model))
  return(list(response = response, theta = new_theta, rank = new_rank, rankTotal = nrow(extended_data), model = refitted_model))
}

score_rank_person <- function(response_vector) {
    # Load required libraries
    library("eRm")
    rasch_model <- readRDS("./rasch_model.rds")

    result <- calc_theta_refit(rasch_model, response_vector)
    # return(list(respose = result$response, theta = result$theta, rank = result$rank, rankTotal = result$rankTotal))
    return(toJSON(list(respose = result$response, theta = result$theta, rank = result$rank, rankTotal = result$rankTotal)))
}

# calc_theta_mle <- function(response_vector, item_difficulties, interval = c(-6, 6)) {
#   # Convert vector to numeric
#   response_vector <- as.numeric(response_vector)
  
#   # Check length
#   if (length(response_vector) != length(item_difficulties)) {
#     stop("Length of response_vector does not match number of items in the model.")
#   }
  
#   # Handle extreme scores (all 0 or all 1)
#   if (all(response_vector == 0) || all(response_vector == 1)) {
#     warning("Extreme score detected: theta cannot be estimated using MLE.")
#     return(NA)
#   }
  
#   # Log-likelihood function for theta
#   loglik_theta <- function(theta, responses, item_difficulty) {
#     sum(responses * log(plogis(theta - item_difficulty)) +
#         (1 - responses) * log(1 - plogis(theta - item_difficulty)))
#   }
  
#   # Optimize to find theta that maximizes log-likelihood
#   opt_result <- optimize(f = function(theta) -loglik_theta(theta, response_vector, item_difficulties),
#                          interval = interval)
  
#   return(opt_result$minimum)
# }

# # Scoring function using item difficulties and raw score conversion
# calc_theta_estimate <- function(response_vector, item_difficulties) {
#   # Check length
#   if (length(response_vector) != length(item_difficulties)) {
#     stop("Response vector and item difficulties must have the same length.")
#   }
  
#   # Compute raw score
#   raw_score <- sum(response_vector, na.rm = TRUE)
  
#   # Estimate theta using a simple Rasch formula (logit approximation)
#   # theta ≈ log(raw_score / (max_score - raw_score)) + mean(item_difficulties)
#   max_score <- length(response_vector)
  
#   if (raw_score == 0) {
#     return(-Inf)  # person got all wrong
#   } else if (raw_score == max_score) {
#     return(Inf)   # person got all correct
#   }
  
#   theta <- log(raw_score / (max_score - raw_score)) + mean(item_difficulties)
#   return(theta)
# }

# score_person <- function(response_vector) {
#     # Load required libraries
#     library("eRm")
#     rasch_model <- readRDS("./rasch_model.rds")

#     # Extract item difficulties from RM model
#     item_difficulties <- unlist(unname(as.list(rasch_model$betapar)))

#     # Calculate theta
#     theta <- calc_theta_refit(rasch_model, response_vector)$theta
#     # theta <- calc_theta_mle(response_vector, item_difficulties)
#     # theta <- calc_theta_estimate(response_vector, item_difficulties)

#     return(theta)
# }

# rank_person <- function(response_vector) {
#     # Load required libraries
#     library("eRm")
#     rasch_model <- readRDS("./rasch_model.rds")

#     # Extract person theta values
#     pp <- person.parameter(rasch_model)
#     theta_values <- pp$theta.table$`Person Parameter`
#     sorted_theta_values <- sort(theta_values, decreasing = TRUE)

#     # Calculate person's theta
#     theta <- score_person(response_vector)

#     return(get_rank(theta, sorted_theta_values))
# }