#' print.mars
#'
#' @param x
#' @param digits
#' @param ...
#'
#' @return
#' @export
#'
print.mars <- function (x, digits = max(3L, getOption("digits") - 3L), ...)
{
  Bfuncs <- x$Bfuncs
  Bfuncs_names <- colnames(x$B)
  cat("Basis functions: ", "\n")
  for (i in 2:length(Bfuncs)) {
    cat(Bfuncs_names[i], "\n")
    print(Bfuncs[[i]])
    cat("\n")
  }
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2L,
                  quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}
