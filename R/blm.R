
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param prior   The prior function for the distribution
#' @param beta    A number
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#'
#' @import stats
#' @export
blm <- function(model, prior, beta, ...){
  if (beta <= 0) stop("beta must be positive!")
  posterior <- update(model, prior, beta, ...)
  object <- structure(list(mean = posterior$mean,
                           Sigma = posterior$Sigma,
                           data = posterior$data,
                           model = model,
                           beta = beta,
                           posterior = posterior,
                           Call = sys.call()),
                      class = "blm")
  object
}
