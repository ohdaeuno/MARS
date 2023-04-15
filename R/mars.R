#' Multivariate Adaptive Regression Splines (MARS)
#'
#' Fit Friedman's Multivariate Adaptive Regression Splines (MARS) model.
#'
#' mars
#'
#' @param formula an R formula
#' @param data a data frame containing the data
#' @param control an object of class 'mars.control'
#'
#' @return an object of class 'mars.control'
#' @export
#'

mars <- function(formula,data,control=mars.control()) {
  cc <- match.call()
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1,drop=FALSE]
  x_names <- colnames(x)
  control <- validate_mars.control(control)
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B))
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
  class(out) <- c("mars",class(fit))
  out
}

fwd_stepwise <- function(y, x, mc) {

  N <- length(y)
  n <- ncol(x)
  Mmax <- mc$Mmax
  B <- init_B(N,Mmax)

  Bfuncs <- vector("list", length = Mmax+1)

  for(i in 1:(Mmax/2)) {
    M = 2*i-1
    lof_best <- Inf
    for(m in 1:M) {

      vset <- setdiff(1:ncol(x), Bfuncs[[m]][,"v"])

      for(v in vset) {

        tt <- split_points(x[,v],B[,m])
        for(t in tt) {
          Bnew <- data.frame(B[,(1:M)],
                             Btem1=B[,m]*h(+1, x[,v], t),
                             Btem2=B[,m]*h(-1, x[,v], t))
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat, mc)
          if(lof < lof_best) {
            lof_best <- lof
            best_split <- c(m=m,v=v,t=t)

          } # end if
        } # end loop over splits
      } # end loop over variables
    } # end loop over basis functions to split

    mstar <- best_split["m"]; vstar <- best_split["v"]; tstar <- best_split["t"]
    B[,M+1] <- B[,mstar]*h(-1, x[,vstar], tstar)
    B[,M+2] <- B[,mstar]*h(+1, x[,vstar], tstar)
    Bfuncs[[M+1]] = rbind(Bfuncs[[mstar]], c(s= -1, vstar, tstar))
    Bfuncs[[M+2]] = rbind(Bfuncs[[mstar]], c(s= +1, vstar, tstar))
  } # end loop over M
  names(y)[1:length(y)] <- c(1:length(y))
  return(list(y=y, B=as.data.frame(B), Bfuncs=Bfuncs))
}

bwd_stepwise <- function(output, mc) {
  y <- output$y
  Mmax <- ncol(output$B)-1
  Jstar <- 2:(Mmax+1)
  dat <- data.frame(y=output$y, output$B)
  lofstar <-LOF(y~., dat, mc)
  Kstar <- Jstar

  for (M in (Mmax+1):2) {
    L <- Kstar
    b <- Inf
    for (m in L) {
      K <- setdiff(L, m)
      df <- data.frame(y=output$y, output$B[, K])
      lof <- LOF(y~., df, mc)
      if(lof < b) {
        b <- lof
        Kstar <- K
      }
      if (lof < lofstar) {
        lofstar <- lof
        Jstar <- K
      }
    }

  }
  Jstar <- c(1, Jstar)
  return(list(y=y, B=output$B[, Jstar], Bfuncs = output$Bfuncs[Jstar]))
}

init_B <- function(N,Mmax) {

  B <- data.frame( matrix(NA,nrow=N,ncol=(Mmax+1)) )
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

LOF <- function(form, data, mc) {
  ff <- lm(form,data)
  rss <- (sum(residuals(ff)^2))
  N <- nrow(data)
  M <- length(coefficients(ff))-1
  CM <- sum(hatvalues(ff))
  d <- mc$d
  CM_tilde <- sum(CM, (d*M))
  res <- (rss*N/(N-(CM_tilde))^2)
  return(res)
}


h <- function(s, x, t) {
  return(pmax(0,s*(x-t)))
}

split_points <- function(xv, Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}



new_mars.control <- function(control) {
  structure(control, class="mars.control")
}

validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax), is.numeric(control$d), is.logical(control$trace))
  if (control$Mmax < 2) warning("Mmax must be >= 2.")
  if (control$Mmax %% 2 > 0) warning("Mmax must be an even integer.")
  control
}


#' mars.control
#'
#' @param Mmax Maximum number of basis functions. Should be an even integer. Default value is 2.
#' @param d Parameter d in GCV. Default value is 3.
#' @param trace Boolean to print details of the fitting process. Default value is FALSE.
#'
#' @return an object of class 'mars.control'
#' @export
#'

mars.control <- function(Mmax=2, d=3, trace=FALSE) {
  Mmax <- as.integer(Mmax)
  if ((Mmax <= 1) || (Mmax %% 2 > 0)) warning("Mmax should be an even integer >= 2")
  control <- list(Mmax=Mmax, d=d, trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}



