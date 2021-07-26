#' Bayesian Kernel Machine Regression
#'
#' @param formula formula to specify design matrix X
#' @param data data for design matrix X
#' @param P covariance distance matrix
#' @param sigma_a sigma normal prior mean
#' @param sigma_b sigma normal prior scale 
#' @param phi_a phi normal prior mean
#' @param phi_b phi normal prior scale 
#' @param est_phi boolean value indicating whether phi should be estimated
#' @param ... optional arguments passed to \code{\link[rstan]{sampling}}
#' @return  bkmreg object
#' @export
#' @importFrom stats model.matrix model.response
#'
bkmr2 <- function(formula,data,P,
                  sigma_a = 1,
                  sigma_b = 3,
                  phi_a = 1,
                  phi_b = 3,
                  est_phi = FALSE,
				  ...)
{

    stopifnot(nrow(P)==ncol(P))
    call <- match.call()
    if(missing(data))
      data <- environment(formula)
    mf <- match.call(expand.dots=FALSE)
    m <- match(c("formula","data"),names(mf),0L)
    mf <- mf[c(1L,m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <-  quote(stats::model.frame)
    mf <- eval(mf,parent.frame())
    mt <- attr(mf,"terms")
    y <- model.response(mf,"any")
    n <- y[,2]
    y <- y[,1]
    X <- model.matrix(mt,mf)

    M <- get_M(nrow(P),nrow(X))
	qrc <- qr(X)
	Q <- qr.Q(qrc)
	R <- qr.R(qrc)
	R_inv <- solve(R)

    standata <- list(N = nrow(X),
                     p = ncol(X),
                     m = nrow(P),
                     est_phi = (est_phi)*1L,
                     y = y,
                     n = n,
					 Q = Q,
					 R_inv = R_inv,
                     P = P^2,
                     M = M,
                     sigma_a = sigma_a,
                     sigma_b = sigma_b,
                     phi_a = phi_a,
                     phi_b = phi_b)

    sampling_args <- set_sampling_args(object = stanmodels$bkmr,
									   control = list(adapt_delta = 0.85,
													  max_treedepth = 10),
									   pars = c("beta","sigma",if(est_phi) "phi","h",'eta_hat'),
									   data = standata,
									   show_messages = FALSE,
									   save_warmup = FALSE,...)


    fit <- do.call(sampling,sampling_args)


    out <- list(fit = fit,
                call = call,
                X = X,
                P = P,
                y = model.response(mf,"any"))
    out <- bkmreg(out)

}

# Internal ------------------------------

set_sampling_args <- function(object,
							  user_dots = list(),
                               ...) {
  args <- list(object = object, ...)
  unms <- names(user_dots)
  for (j in seq_along(user_dots)) {
    args[[unms[j]]] <- user_dots[[j]]
  }
  args$control <- list(adapt_delta = 0.85,
					   max_treedepth = 10)

  return(args)
}

get_M <- function(m,n){
  stopifnot(n>=m)
  M <- diag(n)[,1:m]
}
