% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_degroot.R
\name{tidyDegroot}
\alias{tidyDegroot}
\title{Converts predictions from degroot into a tidy dataset}
\usage{
tidyDegroot(pred, id)
}
\arguments{
\item{pred:}{a matrix of predictions}

\item{id:}{a vector whose length is nrow(pred) that gives ID values for pred.
IF it is missing, it will be set to 1:nrow(pred)}
}
\value{
A tibble (i.e., a data.frame) with 3 columns: an id column (id),
a column for the predicted value from the simulation, and a value for the 
number of iterations, and a column for the number of draws
}
\description{
Converts predictions from degroot into a tidy dataset
}
\note{
This function is in beta, please use with caution!  It also involves
loading several other dependencies, which isn't ideal.
}
