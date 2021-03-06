% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infix_functions.R
\name{like}
\alias{like}
\alias{\%like\%}
\title{String Matching}
\usage{
string \%like\% pattern
}
\value{
TRUE or FALSE value whether an element is like and element in an object or not.
}
\description{
Functions to match easily match strings, based on SQL wildcards.
}
\details{
%like% is an infix version of the like operator in SQL. Provides an easier, more readable wrapper for the grepl function with a single pattern. %not like% provides the opposite functionionality.
}
\examples{
c("bear", "beat", "boat") \%like\% "bea"
[1]  TRUE  TRUE FALSE
}
