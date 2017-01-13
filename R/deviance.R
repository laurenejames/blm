
#' Deviance
#'
#' Sum of the squared distances from the predicted response variable to the observed variable.
#'
#' @param object   The Bayesian linear regression model
#' @param ...   Additional data, for example a data frame.
#'
#' @return deviance
#' @import stats
#' @export
deviance.blm = function(object, ...){
  dev = sum(residuals(object)^2)
  return(dev)
}