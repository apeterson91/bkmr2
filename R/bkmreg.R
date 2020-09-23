#' Bayesian Kernel Machine Regression
#'
#' @param x output from bkmr2
#' @return bkmreg object
bkmreg <- function(x){

  stanfit <- x$fit
  hs <- colMeans(rstan::extract(stanfit,'h')$h)
  betas <- colMeans(rstan::extract(stanfit,'beta')$beta)
  sigma <- mean(rstan::extract(stanfit,'sigma')$sigma)
  yhat <- colMeans(rstan::extract(stanfit,"yhat")$yhat)
  out <- list(coef = betas,
              P = x$P,
              hs = hs,
              y = x$y,
              yhat = yhat,
              sigma = sigma,
              stanfit = stanfit)
  structure(out,class="bkmreg")
}
