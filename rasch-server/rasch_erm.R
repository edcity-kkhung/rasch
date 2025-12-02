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