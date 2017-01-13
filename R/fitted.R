
#' Fitted
#'
#' Fitted response variable, fitted to the predictions that the Bayesian Linear Model makes
#'
#' @param object   The Bayesian linear regression model
#' @param ...      Additional data, for example a data frame (not used)
#'
#' @return Fitted response variables
#' @import stats
#' @import MASS
#' @export
fitted.blm = function(object, ...){
  phiX = model_matrix_responseless(object$model, object$data)
  SigmaX = object$Sigma
  MeanX = object$mean

  fitResults = as.data.frame(matrix(nrow = nrow(phiX), ncol = 2))
  colnames(fitResults) = c("New Prediction","Sigma")
  for (i in 1:nrow(fitResults)){
    fitResults[i,"New Prediction"] = t(MeanX)%*%phiX[i,]
    fitResults[i,"Sigma"] = 1/object$beta + t(phiX[i,]) %*% SigmaX %*% phiX[i,]
  }
  return(fitResults)
}