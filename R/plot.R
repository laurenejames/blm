
#' Plot
#'
#' Plots the Bayesian linear model, the residuals vs the fitted values, similar to in a simple linear model
#'
#' @param x   The Bayesian linear regression model
#' @param ...     Additional data, for example a data frame.
#'
#' @return plot
#' @import stats
#' @import graphics
#' @export
plot.blm = function(x, ...){
  frame = data.frame(Fitted = fitted(x)$"New Prediction", Residuals = residuals(x))
  newframe = frame[order(frame$Fitted),]
  plot(newframe$Residuals, newframe$Fitted, main = "Residuals vs. Fitted", xlab = "Fitted", ylab = "Residuals", type = "o", col = "blue")
}