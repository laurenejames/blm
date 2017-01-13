
#' Coefficients
#'
#' Returns the coefficients of the Bayesian linear model
#'
#' @param object   The Bayesian linear regression model
#' @param ...      Additional data, for example a data frame.
#'
#' @return Fitted parameters for the model
#' @import stats
#' @export
coef.blm = function(object, ...){
  return(list(mean = object$mean, Sigma = object$Sigma))
}