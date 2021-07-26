#' Plotting function for bkmreg objects
#' 
#'
#' @param x bkmreg object
#' @param plotfun one of c("h_dist","ppc")
#' @param ... optional arguments for `plotfun`
#' @export
#'
plot.bkmreg <- function(x,plotfun="h_dist",...){

  p <- switch(plotfun,
         "h_dist" = h_dist(x,...),
         "ppc" = ppc(x,...),
         stop("Not an available plot function"))
  return(p)
}

#' |h| as a function of distance in P
#'
#' @param x bkmreg object
#' @param num_sample number of sample indices to show
#' @export
h_dist <- function(x,num_sample = 3){
  UseMethod("h_dist")
}

#' @export
#' @describeIn h_dist bkmreg h dist plot
#' @importFrom ggplot2 ggplot aes geom_point theme_bw facet_wrap
h_dist.bkmreg <- function(x,num_sample = 3){

	## To pass R CMD CHECK
	Distance <- h <- NULL

  P <- x$P
  sample_ics <- sample(1:nrow(P),num_sample)
  pltdf <- purrr::map_dfr(sample_ics,function(z) dplyr::tibble(index = z,
                                               Distance = P[z,],
                                               h = x$hs))

  p <- pltdf %>% ggplot(aes(x=Distance,y=h)) + geom_point() + theme_bw() +
    facet_wrap(~index) + ggplot2::theme(strip.background=ggplot2::element_blank())

	return(p)

}

#' Posterior Predictive Checks
#'
#' @export
#' @param x bkmrreg object
#' @param num_reps number of replications to show
ppc <- function(x,num_reps = 10){
  UseMethod("ppc")
}


#' @describeIn ppc  bkmr posterior predictive check plot
#' @export
ppc.bkmreg <- function(x,num_reps = 10){

	## To pass R CMD CHECK
  Samples <- Parameter <- iteration_ix <- . <-  NULL


  if(num_reps > nsamples(x) )
    stop("Not enough iterations to display this many samples")



  df <- rstan::extract(x$stanfit,"yhat")$yhat
  samp <- sample(1:nrow(df),num_reps)
  df <- df[samp,]
  colnames(df) <- paste0("ix_",1:ncol(df))
  df <- dplyr::as_tibble(df)
  df$iteration_ix <- samp
  pltdf <- df %>% tidyr::gather(dplyr::contains("ix_"),key="Index",value="Samples") %>%
    dplyr::mutate(Parameter = "yhat") %>% rbind(.,dplyr::tibble(Index = 1:length(x$y[,1]),
                                                         iteration_ix = 0,
                                                         Parameter = "y_obs",
                                                         Samples= x$y[,1]))


  pltdf %>% ggplot2::ggplot(ggplot2::aes(x=Samples,
                                 color=Parameter,
                                 group=iteration_ix)) +
    ggplot2::geom_density() +
    ggplot2::theme_bw()+ ggplot2::theme(legend.title=ggplot2::element_blank()) +
    ggplot2::scale_colour_manual(values=c("black","grey")) +
    ggplot2::scale_alpha_discrete(range=c(1,.1))
}

