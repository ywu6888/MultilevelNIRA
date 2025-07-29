#' Multilevel Ising Network Modeling
#'
#' @param data A data frame or matrix where rows represent observations and columns represent binary (0/1) variables, each variable corresponding to a node.
#' @param group.col A character vector specifying the name(s) of the grouping column(s) used to define the multilevel structure. Default is `c("city")`.
#' @param rnd A list specifying the random-effects structure. Names of list elements correspond to grouping variables, and values are formulas (e.g., `~1`). Default is `list(city = ~1)`.
#' @param family A string specifying the distribution family; currently only `"binomial"` is supported. Default is `"binomial"`.
#' @param AND A logical value indicating whether to use the AND rule to compute the weight (adjacency) matrix. Default is `TRUE`.
#' @param gamma A numeric value for the EBIC penalty parameter γ, which controls model sparsity. Default is `0.25`.
#' @param m An integer used to increase λ_max, thereby extending the range of the penalty parameter sequence. Default is `1`.
#' @param lambda_grid A numeric vector of user-specified penalty parameters λ; if `NULL`, the grid is generated automatically.
#' @param nlambda An integer specifying the number of points in the automatically generated λ sequence. Default is `100`.
#' @param lambda.min.ratio A numeric value giving the ratio of minimum λ to maximum λ; a smaller ratio is used when the number of variables exceeds the number of observations. Default is `ifelse(nrow(data) < ncol(data), 1e-2, 1e-4)`.
#' @param plot A logical value indicating whether to plot the model-selection path. Default is `TRUE`.
#' @param progressbar A logical value indicating whether to display an iteration progress bar. Default is `TRUE`.
#' @param min_sum A numeric lower bound on the sum of conditional log-likelihoods for each node; can be used to adjust for extreme samples. Default is `-Inf`.
#' @param lowerbound.lambda A numeric value to manually set the lower bound for λ; if `NA`, the automatically computed lower bound is used. Default is `NA`.
#' @examples Multilevel_Isingfit(data, group.col =  c("city"), rnd = list(city = ~1),m=1,nlambda = 100)
#' @author Written by Wu Yiming 2025/07/29
#' MultilevelNIRA
#' School of Psychology, South China Normal University
#' @export
Multilevel_Isingfit <- function(data,
                            group.col = c("city"),
                            rnd = list(city = ~1),
                            family = "binomial",
                            AND = TRUE,
                            gamma = 0.25,
                            m=1,
                            lambda_grid = NULL,
                            nlambda = 100,
                            lambda.min.ratio = ifelse(nrow(data) < ncol(data), 1e-2, 1e-4),
                            plot = TRUE,
                            progressbar = TRUE,
                            min_sum = -Inf,
                            lowerbound.lambda = NA,
                            ...) {
  ##— Check Dependencies —##
  if (!requireNamespace("glmmLasso", quietly = TRUE)) {
    stop("Please install the glmmLasso package: install.packages('glmmLasso')")
  }
  library(glmmLasso)
  t0 <- Sys.time()

  ##— Parameters & Data Validation —##
  if (family != "binomial") stop("Only binary (binomial) data is supported.")
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!all(group.col %in% names(data))) {
    stop(paste("Grouping variable columns not found:",
               paste(group.col[!group.col %in% names(data)], collapse = ", ")))
  }
  # Extract grouping and remove grouping columns
  group <- lapply(data[group.col], as.factor)
  x_df  <- data[, setdiff(names(data), group.col), drop = FALSE]
  if (any(is.na(x_df))) stop("Missing values are not supported.")

  # Predictors must be binary 0/1
  for (j in seq_len(ncol(x_df))) {
    v <- x_df[[j]]
    if (!all(is.na(v) | v %in% c(0,1))) {
      stop("Predictors must be binary (0/1) data.")
    }
  }
  if (any(rowSums(x_df, na.rm = TRUE) < min_sum)) {
    stop("There are rows with sum < min_sum.")
  }
  x <- as.matrix(x_df)

  # Keep variables with sufficient variance
  allowedNodes <- function(v) {
    tb <- table(v)
    min(tb) > 1 && max(tb) < length(v) - 1
  }
  keep <- apply(x, 2, allowedNodes)
  if (!any(keep)) stop("All variables have insufficient variance; cannot proceed.")
  x <- x[, keep, drop = FALSE]

  ##— Basic Information —##
  nvar <- ncol(x)
  p    <- nvar - 1

  ##— Preallocate Space for Random Effects —##
  num_groups <- length(group)
  intercepts <- vector("list", nvar)
  betas      <- vector("list", nvar)
  EBIC_mat   <- matrix(NA, nrow = nlambda, ncol = nvar)
  J_mat      <- matrix(0,  nrow = nlambda, ncol = nvar)
  logLik_mat <- matrix(NA, nrow = nlambda, ncol = nvar)
  logLik_pen_mat <- matrix(NA, nrow = nlambda, ncol = nvar)  # new: penalized log-likelihood
  N_vec      <- numeric(nvar)
  randomsIn          <- vector("list", num_groups)
  randomsIntercept   <- vector("list", num_groups)

  for (i in seq_len(nvar)) {
    intercepts[[i]] <- numeric(nlambda)
    betas[[i]]      <- matrix(NA, nrow = nlambda, ncol = nvar-1)
  }
  for (g in seq_len(num_groups)) {
    G <- nlevels(group[[g]])
    randomsIn[[g]]        <- lapply(seq_len(nvar), function(i) matrix(NA, nrow = nlambda, ncol = G))
    randomsIntercept[[g]] <- lapply(seq_len(nvar), function(i) numeric(G))
  }

  if (progressbar) pb <- txtProgressBar(min = 0, max = nvar, style = 3)

  ##— Main Loop: Modeling for each node i —##
  for (i in seq_len(nvar)) {
    y  <- x[, i]
    Xi <- x[, -i, drop = FALSE]
    N_i <- length(y)
    N_vec[i] <- N_i

    df <- data.frame(y = y, as.data.frame(Xi), as.data.frame(group))
    covariates <- colnames(Xi)

    # —— Automatically generate lambda_grid —— #
    if (is.null(lambda_grid)) {
      null_fit <- lme4::glmer(
        y ~ 1 + (1|city),
        data   = df,
        family = binomial(link="logit")
      )
      mu0 <- as.vector(fitted(null_fit))
      w   <- mu0 * (1 - mu0)

      # compute grads & lambda_grid
      res    <- y - mu0
      grads  <- abs(colSums(Xi * (w * res)))
      lam_max <- max(grads)
      lam_min <- lam_max * lambda.min.ratio
      lam_max2 <- m* lam_max
      lam_min <- lam_max * lambda.min.ratio
      my_lambda_grid <- exp(seq(log(lam_max2), log(lam_min), length = nlambda))
    } else {
      my_lambda_grid <- lambda_grid
    }
    if (length(my_lambda_grid) != nlambda) {
      nlambda <- length(my_lambda_grid)
    }

    if (progressbar) {
      cat(sprintf("\nNode %d: lambda sequence from %.3g to %.3g, %d points\n",
                  i, max(my_lambda_grid), min(my_lambda_grid), length(my_lambda_grid)))
    }

    # Within-cluster centering
    cluster_id <- interaction(df[group.col], drop = TRUE)
    for (v in covariates) {
      df[[v]] <- df[[v]] - ave(df[[v]], cluster_id)
    }

    for (j in seq_along(my_lambda_grid)) {
      lam <- my_lambda_grid[j]
      fit <- glmmLasso::glmmLasso(
        fix      = as.formula(paste("y ~", paste(covariates, collapse = " + "))),
        rnd      = rnd,
        data     = df,
        family   = binomial(link = "logit"),
        lambda   = lam,
        switch.NR = TRUE,
        final.re = TRUE
      )
      coefs <- fit$coeff
      intercepts[[i]][j] <- coefs["(Intercept)"]
      betas[[i]][j, ]    <- coefs[covariates]

      # random effects
      ran <- fit$ranef
      for (g in seq_len(num_groups)) {
        G <- nlevels(group[[g]])
        grp_names <- paste0(group.col[g], 1:G)
        randomsIn[[g]][[i]][j, ] <- ran[grp_names]
      }

      # save original logLik
      logLik_mat[j, i] <- as.numeric(fit$loglik)

      # new: compute and save penalized log-likelihood
      L0     <- as.numeric(fit$loglik)
      beta_p <- coefs[-1]  # drop intercept
      logLik_pen_mat[j, i] <- L0 - lam * sum(abs(beta_p))

      J_mat[j, i] <- sum(coefs[covariates] != 0)

      if (progressbar && (j %% 10 == 0)) {
        cat(sprintf("  lambda=%.3g → J=%d, logLik=%.2f, penLL=%.2f\n",
                    lam, J_mat[j,i], logLik_mat[j,i], logLik_pen_mat[j,i]))
      }
    }

    # compute EBIC
    penalty        <- J_mat[, i] * log(N_i) + 2 * gamma * J_mat[, i] * log(p)
    EBIC_mat[, i]  <- -2 * logLik_pen_mat[, i] + penalty

    if (progressbar) setTxtProgressBar(pb, i)
  }
  if (progressbar) close(pb)

  ##— Select optimal λ and assemble network —##
  lambda_idx <- apply(EBIC_mat, 2, which.min)
  lambda_opt <- sapply(seq_len(nvar), function(i) {
    if (is.null(lambda_grid)) my_lambda_grid[lambda_idx[i]] else lambda_grid[lambda_idx[i]]
  })

  weights    <- matrix(0, nrow = nvar, ncol = nvar)
  thresholds <- numeric(nvar)
  EBIC_best  <- numeric(nvar)
  for (i in seq_len(nvar)) {
    idx <- lambda_idx[i]
    thresholds[i]   <- intercepts[[i]][idx]
    weights[i,-i]   <- betas[[i]][idx, ]
    EBIC_best[i]    <- EBIC_mat[idx, i]
    for (g in seq_len(num_groups)) {
      randomsIntercept[[g]][[i]] <- randomsIn[[g]][[i]][idx,]
    }
  }

  symmetrize_matrix <- function(W, AND = TRUE) {
    n <- nrow(W)
    Wf <- W
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        a <- W[i,j]; b <- W[j,i]
        if (AND) {
          Wf[i,j] <- Wf[j,i] <- if (a==0 || b==0) 0 else (a+b)/2
        } else {
          Wf[i,j] <- Wf[j,i] <- if (a==0 && b!=0) b else if (b==0 && a!=0) a else (a+b)/2
        }
      }
    }
    diag(Wf) <- 0
    Wf
  }
  W_final <- symmetrize_matrix(weights, AND = AND)

  # Plotting
  doPlot <- !identical(plot, FALSE)
  qobj   <- qgraph::qgraph(W_final, layout = "spring", DoNotPlot = !doPlot, ...)

  # Return results
  Res <- list(
    weiadj           = W_final,
    thresholds       = setNames(thresholds, colnames(x)),
    lambda_opt       = setNames(lambda_opt, colnames(x)),
    gamma            = gamma,
    AND              = AND,
    time             = Sys.time() - t0,
    randomsIntercept = setNames(randomsIntercept, group.col),
    EBIC_mat         = EBIC_mat,
    Best_EBIC        = setNames(EBIC_best, colnames(x)),
    logLik_pen_mat   = logLik_pen_mat,   # penalized log-likelihood
    q                = qobj
  )
  class(Res) <- "IsingFit_ml"
  return(Res)
}

