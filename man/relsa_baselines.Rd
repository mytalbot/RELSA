% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relsa_baselines.R
\name{relsa_baselines}
\alias{relsa_baselines}
\title{Baseline calculation}
\usage{
relsa_baselines(dataset = NULL, bslday = -1, variables = NULL, turnvars = NULL)
}
\arguments{
\item{dataset}{sample data frame}

\item{bslday}{time point in the data frame used for baseline calculation (i.e. day=-1)}

\item{variables}{variables to use as references (must be loaded!)}

\item{turnvars}{variable names with "turned" direction (e.g. c("hr","temp"))}
}
\value{
\code{baseline} list with reference variables set to 100 \%, maximum reached values and some characteristics
}
\description{
The \code{relsa_baselines} function calculates baseline levels for the provided reference set.
}
