
#' Model matrix responseless.
#'
#' Fits a model with a responseless matrix, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame.
#'
#' @return Model matrix
#' @import stats
#' @import MASS
#' @export
model_matrix_responseless <- function(model, ...){
  responseless <- delete.response(terms(model))
  data <- model.frame(responseless, ...)
  result <- model.matrix(responseless, data)
  return(result)
}