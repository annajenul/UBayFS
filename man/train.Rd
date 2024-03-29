% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trainUBaymodel.R
\name{train}
\alias{train}
\title{UBayFS feature selection}
\usage{
train(x, verbose = FALSE)
}
\arguments{
\item{x}{a `UBaymodel` created by \link{build.UBaymodel}}

\item{verbose}{if TRUE: GA optimization output is printed to the console}
}
\value{
a `UBaymodel` with an additional list element output containing the optimized solution.
}
\description{
Genetic algorithm to train UBayFS feature selection model.
}
