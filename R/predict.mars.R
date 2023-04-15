#' predict.mars
#'
#' @param object
#' @param newdata
#'
#' @return
#' @export
#'
predict.mars <- function(object, newdata) {
  if (missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame (tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix (mt, mf) [, -1]

    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop (B %*% beta)
}

make_B <- function(X, Bfuncs) {
  B <- rep(1, nrow(X))

  for (i in 2:(length(Bfuncs))) {
    s <- Bfuncs[[i]][, "s"]
    v <- Bfuncs[[i]][, "v"]
    t <- Bfuncs[[i]][, "t"]
    res <- rep(1, nrow(X))

    for (j in 1:length(s)) {
      res <- res*h(s[j], X[, v[j]], t[j])
    }

    B <- cbind(B, res)

  }
  return(B)
}
