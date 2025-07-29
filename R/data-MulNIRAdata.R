#' Multilevel NIRA Example Data
#'
#' A subset of the `PBICR-2023 Anxiety data` dataset (first 1000 rows) used for demonstrating
#' the multilevel Ising-network fitting functions in this package.
#'
#' @format A data frame with 1000 rows and 8 columns, where:
#' \describe{
#'   \item{NA,UW,WTM, …, ASH}{Symptoms in GAD-7,Binary variables (0/1) serving as network nodes.}
#'   \item{city}{Factor indicating the grouping variable (city).}
#' }
#' @source Preprocessed from original PBICR-2023 Anxiety data.
#' @docType data
#' @keywords datasets
#' @name MulNIRAdata
#' @usage data(MulNIRAdata)
NULL
.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    crayon::blue("     ／＞　 フ\n"),
    crayon::blue("    | 　_　_| "), crayon::green("Meow! 🐾\n"),
    crayon::blue("  ／` ミ＿xノ  "), crayon::magenta("MultilevelNIRA loaded!\n"),
    crayon::blue(" /　　　　 |   "), crayon::yellow("Installation complete 🎉\n"),
    crayon::blue("/　 ヽ　　 ﾉ\n"),
    crayon::blue("│　　|　|　|\n"),
    crayon::blue("／￣|　　 |　|"), crayon::red("  Enjoy using it!\n"),
    crayon::blue("`ー┴─┴─´")
  )
  packageStartupMessage(msg)
}

