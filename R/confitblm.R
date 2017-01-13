
#' Confidence Intervals
#'
#' Calculates the confidence intervals for parameters fitted in the Bayesian linear model
#'
#' @param object   The Bayesian linear regression model
#' @param parm  The parameter(s) in question
#' @param level The confidence level required, default gives 95\% CIs
#' @param ...   Additional data, for example a data frame.
#'
#' @return confidence intervals on fitted parameters
#' @import stats
#' @export
confint.blm = function(object, parm, level = 0.95, ...) {
  tables = as.data.frame(object$mean)
  colnames(tables) = "Mean"
  lower = (1-level)/2
  upper = level + (1-level)/2
  tables[,as.character(lower)] = 0
  tables[,as.character(upper)] = 0
  for (i in 1:nrow(tables)){
    tables[i, c(as.character(lower), as.character(upper))] = qnorm(c(lower, upper), mean = tables[i,1], sd = object$Sigma[i,i])
  }
  return(tables)
}