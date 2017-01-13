
#' Predict
#'
#' Makes predictions based on the fitted Bayesian linear model, using new data, without new data it gives predictions ont he data used to fit the model
#'
#' @param object   The Bayesian linear regression model
#' @param newdata  New data set
#' @param beta     Number
#' @param ...      Additional data, for example a data frame.
#'
#' @return fitted model
#' @import stats
#' @export
predict.blm = function(object, newdata, beta, ...) {
  newBlm = blm(object$model, object$posterior, beta, newdata)
  prediction = fitted(newBlm)
  return(prediction)
}