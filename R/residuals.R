
#' Residuals
#'
#' Returns the residual fitted Bayesian model, the difference between the predicted and the observed values.
#'
#' @param object   The Bayesian linear regression model
#' @param ...      Additional data, for example a data frame.
#'
#' @return residuals
#' @import stats
#' @export
residuals.blm = function(object, ...){
  response_var = as.character(formula(object$model)[[2]])
  observed = object$data[,response_var]
  predicted = fitted(object)$"New Prediction"
  residual = observed - predicted
  return(residual)
}