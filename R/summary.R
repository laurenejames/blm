
#' Summary
#'
#' Summarises the features of the Bayesian regression model
#'
#' @param object   The Bayesian linear regression model
#' @param ...   Additional data, for example a data frame.
#'
#' @return summary
#' @export
summary.blm = function(object, ...){
  print(object)
  writeLines("\nResiduals:")
  print(residuals(object))
  writeLines("\nDeviance:")
  print(deviance(object))
}
