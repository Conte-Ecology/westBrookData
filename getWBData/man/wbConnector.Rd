% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wbConnector.R
\name{wbConnector}
\alias{wbConnector}
\title{Connect to West Brook sql database}
\usage{
wbConnector()
}
\value{
\code{con} A link to the database for use with DBI functions

\code{conDplyr} A link to the database for use with dplyr functions
}
\description{
Creates two links to the West Brook database one for DBI and one for dplyr
}
