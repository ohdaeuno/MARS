#' plot.mars
#'
#' @param object
#'
#' @return
#' @export
#'
plot.mars <- function(object) {
  Bnames <- names(object$coefficients)[-1]
  Bcoefs <- object$coefficients
  B <- object$B
  nterms <- ncol(B)
  container <- as.data.frame(matrix(1, nrow = nrow(B), ncol = nterms))

  for(i in 1:nterms){
    container[,i] <- Bcoefs[i]*B[i]
  }
  result <- apply(container, 1, sum)
  plot(1:nrow(B), result, type = "l", main ="The best fitted line", ylab= "y", xlab = "observation")
  points(1:nrow(B), object$y)
  stats:::plot.lm(object)
}
