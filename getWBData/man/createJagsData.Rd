% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createJagsData.R
\name{createJagsData}
\alias{createJagsData}
\title{Create data needed for JAGS runs}
\usage{
createJagsData(coreData, modelType = "CJS")
}
\arguments{
\item{coreData}{a data.frame created using createCoreData}

\item{modelType}{either "CJS" or "JS"}
}
\value{
List of evalRows and firstObsRows
}
\description{
Create data needed for JAGS runs
}
\details{
\strong{Options of columns to add}
}
