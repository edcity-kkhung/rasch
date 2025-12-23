irt_2pl_model_fit <- function(data_vector, nrow, ncol) {
  # Fit IRT model and return results as JSON 
  library("mirt")
  library("jsonlite")
  
#   source("./irt_2pl.R")
  
  # Reshape data vector into matrix
  X <- matrix(data_vector, nrow = nrow, ncol = ncol, byrow = TRUE)
  colnames(X) <- paste0("Item", 1:ncol)  
  
  # Fit and extract results
  res <- irt_2pl(X, verbose = TRUE)
  
#   return(toJSON(list(difficulties = res$item_difficulty, abilities = res$ability)))
  return(res)
}

irt_2pl <- function(
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
      # mirt can handle NAs; they are omitted from the likelihood.
      if (verbose) message("Missing responses will be omitted in estimation.")
    }
  }
  
  # ---- Fit 2PL via mirt ----
  if (!requireNamespace("mirt", quietly = TRUE))
    stop("Package 'mirt' is required. Please install it: install.packages('mirt')")
  
  if (verbose) message("Fitting 2PL model using mirt::mirt (unidimensional)...")
  # itemtype='2PL' fits a 2-parameter logistic model, ability ~ N(0,1)
  fit <- mirt::mirt(data = X, model = 1, itemtype = '2PL', SE = TRUE, verbose = verbose)
  
  # ---- Item parameters (a and b) ----
  # Get IRT parameterization (a, b); also get internal (a, d) for SE delta-method on b.
  cf_irt <- mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)
  cf_int <- mirt::coef(fit, IRTpars = FALSE, simplify = TRUE)
  
  items_irt <- as.data.frame(cf_irt$items)   # columns typically: a, b
  items_int <- as.data.frame(cf_int$items)   # columns typically: a1, d
  
  # Ensure item order matches matrix column names
  item_names <- colnames(X)
  rownames(items_irt) <- rownames(items_int) <- item_names
  
  item_a <- items_irt$a
  item_b <- items_irt$b
  names(item_a) <- names(item_b) <- item_names
  
  # ---- Item SEs via vcov (delta-method for b = -d/a) ----
  # We extract var(a) directly; var(b) via transformation using (a, d) covariance.
  V <- try(mirt::vcov(fit), silent = TRUE)
  item_a_se <- item_b_se <- rep(NA_real_, J)
  
  if (!inherits(V, "try-error")) {
    rn <- rownames(V)
    # Helper to find parameter indices for each item, robust to naming conventions
    find_idx <- function(item, par) {
      # Attempt multiple name styles that mirt may use
      candidates <- c(
        paste0(par, "-", item),
        paste0(par, "_", item),
        paste0(item, "-", par),
        paste0(item, "_", par)
      )
      which(rn %in% candidates)
    }
    
    for (j in seq_len(J)) {
      it <- item_names[j]
      # Internal parameters (for delta-method): a1 (discrimination), d (intercept)
      ida <- find_idx(it, "a1")
      idd <- find_idx(it, "d")
      if (length(ida) == 1) {
        var_a <- V[ida, ida]
        item_a_se[j] <- sqrt(var_a)
      }
      if (length(ida) == 1 && length(idd) == 1) {
        a <- items_int[it, "a1"]
        d <- items_int[it, "d"]
        var_a <- V[ida, ida]
        var_d <- V[idd, idd]
        cov_ad <- V[ida, idd]
        # b = -d/a  =>  ∂b/∂a = d/a^2,  ∂b/∂d = -1/a
        var_b <- (d^2 / a^4) * var_a + (1 / a^2) * var_d - (2 * d / a^3) * cov_ad
        item_b_se[j] <- if (is.finite(var_b) && var_b >= 0) sqrt(var_b) else NA_real_
      }
    }
  } else {
    if (verbose) message("vcov(fit) unavailable; item SEs set to NA.")
  }
  names(item_a_se) <- names(item_b_se) <- item_names
  
  # ---- Person parameters (EAP) ----
  fs <- mirt::fscores(fit, method = "EAP", full.scores.SE = TRUE)
  ability <- as.numeric(fs[, 1])
  ability_se <- as.numeric(fs[, 2])
  
  # ---- Raw scores and extremes (informational) ----
  person_total <- rowSums(X, na.rm = TRUE)
  item_total   <- colSums(X, na.rm = TRUE)
  all0_person  <- person_total == 0
  all1_person  <- person_total == rowSums(!is.na(X))
  all0_item    <- item_total == 0
  all1_item    <- item_total == colSums(!is.na(X))
  # Note: In 2PL MML, extreme persons/items can still be estimated but may have large SEs.
  
  # ---- Output list ----
  list(
    call = match.call(),
    method = "2PL (MML) via mirt::mirt",
    converged = isTRUE(fit@OptimInfo$converged),
    item_discrimination = item_a,       # a_j
    item_discrimination_se = item_a_se,
    item_difficulty = item_b,           # b_j (location)
    item_difficulty_se = item_b_se,
    ability = ability,                  # theta_i (EAP)
    ability_se = ability_se,
    person_total = person_total,
    item_total = item_total,
    all0_person = all0_person,
    all1_person = all1_person,
    all0_item = all0_item,
    all1_item = all1_item,
    fit_object = fit
  )
}
