#' Extract Thresholds from a Multilevel Ising Fit
#'
#' Retrieve fixed or random‑intercept thresholds from an object of class \code{IsingFit_ml}.
#'
#' @param fit The object returned by \code{Multilevel_Isingfit()}, containing the fitted multilevel Ising model results.
#' @param type A string specifying the intercept type to extract: \code{"random"} for random intercepts or \code{"fixed"} for fixed intercepts.
#' @param group A character string naming the grouping variable; required when \code{type = "random"}. This should match the \code{group.col} argument used in \code{Multilevel_Isingfit()}.
#' @param level An integer indicating which level’s random intercept to extract; required when \code{type = "random"}. For example, \code{level = 5} refers to the fifth hierarchical level.
#' @examples Thresholds_select(Mul_Isingmodel, type = "fixed")
#' @examples Thresholds_select(Mul_Isingmodel,type  = "random",group = "city",level = 5)
#' @author Written by Wu Yiming 2025/07/29
#' MultilevelNIRA
#' School of Psychology, South China Normal University

Thresholds_select <- function(fit,
                              type  = c("random", "fixed"),
                              group = NULL,
                              level = NULL) {
  type <- match.arg(type)
  if (!inherits(fit, "IsingFit_ml")) {
    stop("The first argument must be the result returned by IsingFit_multilevel().")
  }
  if (type == "fixed") {
    return(fit$thresholds)
  }
  if (is.null(group) || is.null(level)) {
    stop("When type = 'random', both 'group' and 'level' must be specified.")
  }
  if (!(group %in% names(fit$randomsIntercept))) {
    stop("Grouping variable not found in results: ", group)
  }
  group_list <- fit$randomsIntercept[[group]]

  # Compatible with vectors and matrices
  first_item <- group_list[[1]]
  if (is.matrix(first_item) || is.data.frame(first_item)) {
    G <- ncol(first_item)
  } else {
    G <- length(first_item)
  }
  if (level < 1 || level > G) {
    stop("Level index out of bounds: should be between 1 and ", G, ".")
  }

  # Extraction
  res_vec <- sapply(group_list, function(item) {
    if (is.matrix(item) || is.data.frame(item)) {
      item[1, level]
    } else {
      item[level]
    }
  })
  names(res_vec) <- names(group_list)
  return(res_vec)
}
