
#' Methods for bkmreg objects
#'
#'
#' Explanatory text
#' @name bkmreg-methods
#' @param object bkmreg object
#' @param ... optional arguments
NULL


#' @rdname bkmreg-methods
#' @export
#' @importFrom rstantools nsamples
nsamples.bkmreg <- function(object, ...) {
  nrow(as.matrix(object$stanfit))
}
