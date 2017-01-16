
#' Plot
#'
#' Plots the Bayesian linear model, similar to in a simple linear model
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
  par(mfrow = c(2,2))

  # Set up items for plot
  L = lm(newframe$Residuals~newframe$Fitted)
  st_res = newframe$Residuals/sd(newframe$Residuals)
  root_st_res = sqrt(st_res)
  L2 = lm(root_st_res ~ newframe$Fitted)
  lev = hat(model.matrix(L))
  L3 = lm(st_res ~ lev)

  # Residuals vs fitted
  plot(newframe$Fitted, newframe$Residuals, main = "Residuals vs. Fitted", xlab = "Fitted", ylab = "Residuals", type = "o", col = "blue")
  abline(a = L$coefficients[1], b = L$coefficients[2], col = "red")

  # Q-Q Plot
  qqnorm(newframe$Residuals, main = "Normal Q-Q plot",
         xlab = "Theoretical Qualities",
         ylab = "Standardised Residuals")

  # Scale-location plot
  plot(newframe$Fitted, root_st_res, main = "Scale-Location",
       xlab = "Fitted values",
       ylab = "Square root of standardised residuals")
  abline(a = L2$coefficients[1], b = L2$coefficients[2], col = "red")

  # Residuals vs leverage
  plot(lev, st_res, main = "Residual vs. Leverage",
       xlab = "Leverage", ylab = "Standardised residuals")
  abline(a = L3$coefficients[1], b = L3$coefficients[2], col = "red")
}