% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.blm}
\alias{predict.blm}
\title{Predict}
\usage{
\method{predict}{blm}(object, newdata, beta, ...)
}
\arguments{
\item{object}{The Bayesian linear regression model}

\item{newdata}{New data set}

\item{beta}{Number}

\item{...}{Additional data, for example a data frame.}
}
\value{
fitted model
}
\description{
Makes predictions based on the fitted Bayesian linear model, using new data, without new data it gives predictions ont he data used to fit the model
}

