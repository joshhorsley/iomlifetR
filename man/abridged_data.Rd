% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{abridged_data}
\alias{abridged_data}
\title{Abridged population and mortality data for Australia}
\format{An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 1452 rows and 5 columns.}
\usage{
abridged_data
}
\description{
A dataset containing population and mortality data for (TODO which geographic unit?) age groups 0, 1 - 4, 5 - 9, 10 - 14, ... 100+.
}
\details{
@format A dataframe with 19 rows and 3 variables
 \describe{#'
   \item{Measure}{Population or deaths}
   \item{sex}{Sex}
   \item{age}{Age group}
   \item{time}{Year}
   \item{value}{Mid-year population or annual number of deaths}
 }
 @source \url{http://stat.data.abs.gov.au}
}
\keyword{datasets}
