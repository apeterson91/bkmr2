#' Bayesian Kernel Machine Regression
#'
#' @param x output from bkmr2
#' @return bkmreg object
bkmreg <- function(x){

	stanfit <- x$fit
	hs <- colMeans(rstan::extract(stanfit,'h')$h)
	betas <- colMeans(rstan::extract(stanfit,'beta')$beta)
	sigma <- mean(rstan::extract(stanfit,'sigma')$sigma)
	yhat <- apply(t(sapply(1:nrow(rstan::extract(stanfit,'eta_hat')$eta_hat),function(z) stats::rbinom(n = nrow(x$y),size = x$y[,2], prob = rstan::extract(stanfit,"eta_hat")[[1]][z,]))),2,mean)
	out <- list(coef = betas,
	            P = x$P,
	            hs = hs,
	            y = x$y,
	            yhat = yhat,
	            sigma = sigma,
	            stanfit = stanfit)

  structure(out,class="bkmreg")
}
