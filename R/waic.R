
#' Widely Applicable Information Criterion
#'
#' @importFrom stats dbinom
#' @export
#' @param x bkmreg object
#'
waic <- function(x) UseMethod("waic")

#' @describeIn waic
#' @export
waic.bkmreg <- function(x){

  mu <- rstan::extract(x$stanfit,"eta_hat")[[1]]
	ll <- t(dbinom(x = x$y[,1],prob = mu,size = x$y[,2],log=T))
	out <- LaplacesDemon::WAIC(ll)
	return(out)

}
