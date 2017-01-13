
#' Make prior.
#'
#' Makes a prior distribution for later use in Bayesian Linear Regression
#'
#' @param model   A formula describing the model.
#' @param alpha   A number
#' @param mu      Mean, default set to 0
#'
#' @return A prior distribution
#' @import stats
#' @export
make_prior <- function(model, alpha, mu = c(0,0)) {
  if (alpha <= 0) stop("alpha must be positive!")
  Sigma <- diag(1/alpha, nrow = length(all.vars(model)))
  return(list(Sigma = Sigma, mean = mu))
}