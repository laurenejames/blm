
#' Print
#'
#' Prints the fitted model and its coefficients
#'
#' @param x   The Bayesian linear regression model
#' @param ...   Additional data, for example a data frame.
#'
#' @return print
#' @export
print.blm = function(x, ...){
  print("Call:")
  print(x$Call)
  writeLines("\nCoefficients")
  writeLines("\nMean:")
  print(coef(x)$mean)
  writeLines("\nSigma:")
  print(coef(x)$Sigma)
}