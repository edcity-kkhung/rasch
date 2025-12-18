rasch_erm <- function(
  X,
  na_handling = c("omit", "zero", "fail"),
  verbose = FALSE
) {
  # ---- Validation ----
  na_handling <- match.arg(na_handling)
  if (!is.matrix(X)) stop("Input must be a matrix with persons in rows and items in columns.")
  if (!all(X[!is.na(X)] %in% c(0, 1))) stop("Matrix must contain only 0/1 (or NA).")
  N <- nrow(X); J <- ncol(X)
  if (N < 2 || J < 2) stop("Need at least 2 persons and 2 items.")
  
  # ---- NA handling ----
  if (anyNA(X)) {
    if (na_handling == "fail") {
      stop("NA values present. Set na_handling to 'omit' or 'zero'.")
    } else if (na_handling == "zero") {
      X[is.na(X)] <- 0
    } else if (na_handling == "omit") {
      # eRm can handle NAs; they will be ignored in CML.
      if (verbose) message("Missing responses will be omitted in estimation.")
    }
  }
  
  # ---- Fit Rasch via eRm ----
  if (!requireNamespace("eRm", quietly = TRUE))
    stop("Package 'eRm' is required. Please install it: install.packages('eRm')")
  
  fit <- eRm::RM(X)  # CML for Rasch model
  
  # ---- Item difficulties ----
  # eRm stores item parameters in 'betapar', with sum-to-zero constraint by default.
  item_beta <- as.numeric(fit$betapar) 
  names(item_beta) <- colnames(X)
  
  # SEs for items
  # vcov() returns variance-covariance matrix of item parameters (CML-based).
  V <- try(stats::vcov(fit), silent = TRUE)
  if (inherits(V, "try-error")) {
    item_se <- rep(NA_real_, length(item_beta))
  } else {
    item_se <- sqrt(diag(V))
  }
  
  # ---- Person parameters ----
  pp <- eRm::person.parameter(fit)
  # Abilities (theta) and SEs; extreme scores may be NA
  ability <- as.numeric(pp$theta.table$`Person Parameter`)
  ability_se <- as.numeric(pp$theta.table$`Std. Error`)
  rownames(pp$theta.table) <- NULL
  
  # Person raw scores
  person_total <- rowSums(X, na.rm = TRUE)
  item_total <- colSums(X, na.rm = TRUE)
  
  # Identify extremes (not estimable in CML)
  all0_person <- person_total == 0
  all1_person <- person_total == rowSums(!is.na(X))
  all0_item   <- item_total == 0
  all1_item   <- item_total == colSums(!is.na(X))
  
  # ---- Output list ----
  list(
    call = match.call(),
    method = "Rasch (CML) via eRm::RM",
    converged = TRUE,  # eRm signals issues via warnings/errors; RM doesn't expose a flag
    item_difficulty = item_beta,      # b_j (mean-centered by eRm constraint)
    item_difficulty_se = item_se,
    ability = ability,                # theta_i (person parameters)
    ability_se = ability_se,
    person_total = person_total,
    item_total = item_total,
    all0_person = all0_person,
    all1_person = all1_person,
    all0_item = all0_item,
    all1_item = all1_item,
    fit_object = fit,                 # return the fitted model for further analysis
    person_object = pp
  )
}


# Estimate Rasch ability for ONE new person using fixed item difficulties (no refit).
rasch_theta_ml <- function(
  x,
  item_beta,
  na_handling = c("omit", "zero"),
  theta_bounds = c(-6, 6), tol = 1e-8, maxit = 100
) {
  na_handling <- match.arg(na_handling)
  stopifnot(is.numeric(item_beta), is.numeric(x))
  if (length(x) != length(item_beta))
    stop("Length of x must match length of item_beta (same items, same order).")

  # Handle missing
  if (anyNA(x)) {
    if (na_handling == "zero") {
      x[is.na(x)] <- 0
    } else { # omit
      keep <- !is.na(x)
      x         <- x[keep]
      item_beta <- item_beta[keep]
    }
  }

  # Guard: binary only
  if (!all(x %in% c(0, 1)))
    stop("Responses must be 0/1 (after NA handling).")

  J      <- length(item_beta)
  score  <- sum(x)
  nonmis <- J

  # Extreme scores: ML undefined under Rasch CML
  if (score == 0L)  return(-Inf)
  if (score == nonmis) return(Inf)

  # Score function S(theta) = sum_j (x_j - P_j(theta))
  S <- function(theta) {
    eta <- theta - item_beta
    pj  <- 1 / (1 + exp(-eta))
    sum(x - pj)
  }

  # Root finding
  lower <- theta_bounds[1]
  upper <- theta_bounds[2]

  # Expand bounds if S(lower) and S(upper) have same sign
  sL <- S(lower); sU <- S(upper)
  iter <- 0
  while (sL * sU > 0 && iter < 10) {
    lower <- lower - 2
    upper <- upper + 2
    sL <- S(lower); sU <- S(upper)
    iter <- iter + 1
  }

  if (sL * sU > 0) {
    # Fallback: use Newton steps from 0
    theta <- 0
    for (k in seq_len(maxit)) {
      eta <- theta - item_beta
      pj  <- 1 / (1 + exp(-eta))
      w   <- pj * (1 - pj)
      g   <- sum(x - pj)       # gradient
      H   <- -sum(w)           # Hessian
      step <- -g / H
      theta <- theta + step
      if (abs(step) < tol) break
      theta <- max(min(theta, upper), lower)
    }
    return(theta)
  }

  uniroot(S, interval = c(lower, upper), tol = tol)$root
}


# Calibrate ONE new item difficulty b using fixed person abilities (theta).
# x_new: 0/1 vector of responses to the new item (length = number of persons).
# theta: numeric vector of person abilities on the same scale as your bank.
rasch_calibrate_new_item_ml <- function(
  x_new,
  theta,
  na_handling = c("omit", "zero"),
  bounds = c(-8, 8), tol = 1e-8
) {
  na_handling <- match.arg(na_handling)
  stopifnot(is.numeric(theta), is.numeric(x_new))
  if (length(x_new) != length(theta))
    stop("x_new and theta must have the same length (same persons).")
  
  # Handle missing
  keep <- !is.na(x_new) & !is.na(theta)
  if (na_handling == "omit") {
    x_new <- x_new[keep]; theta <- theta[keep]
  } else if (na_handling == "zero") {
    x_new[is.na(x_new)] <- 0
    theta <- theta[!is.na(theta) & !is.na(x_new)]
    x_new <- x_new[!is.na(theta)]
  }
  
  # Check binary
  if (!all(x_new %in% c(0L, 1L))) stop("x_new must be 0/1 after NA handling.")
  n <- length(x_new)
  if (n < 2) stop("Need at least 2 persons to calibrate an item.")
  
  # Extremes: ML is undefined
  s <- sum(x_new)
  if (s == 0L)  return(list(difficulty = Inf,  se = NA_real_, method = "ML (all 0; b=+Inf)"))
  if (s == n)   return(list(difficulty = -Inf, se = NA_real_, method = "ML (all 1; b=-Inf)"))
  
  # Score function S(b) = sum_i (P_i(b) - x_i)
  S <- function(b) {
    pj <- 1 / (1 + exp(-(theta - b)))
    sum(pj - x_new)
  }
  
  # Root finding
  root <- uniroot(S, interval = bounds, tol = tol)$root
  
  # SE via Fisher information: I(b) = sum_i p_i(1-p_i)
  pj <- 1 / (1 + exp(-(theta - root)))
  info <- sum(pj * (1 - pj))
  se <- 1 / sqrt(info)
  
  list(difficulty = root, se = se, method = "ML (anchored by theta)")
}
