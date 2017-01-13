
#' Update
#'
#' Updates the prior distribution for inputing into the blm model
#'
#' @param model   A formula describing the model.
#' @param prior   The prior distribution
#' @param beta    A number
#' @param ...     Additional data, for example a data frame.
#'
#' @return Update function
#' @import stats
#' @import MASS
#' @export
update <- function(model, prior, beta, ...){
  data <- model.frame(model, as.data.frame(list(...)))
  phiX <- model_matrix_responseless(model, ...)
  Sigma <- solve(prior$Sigma + beta * t(phiX) %*% phiX)
  response_var <- as.character(formula(model)[[2]]) # Extracts name of response variable
  mean <- beta * Sigma %*% t(phiX) %*% data[,response_var]
  return(list(mean=mean, Sigma = Sigma, data = data))
}
