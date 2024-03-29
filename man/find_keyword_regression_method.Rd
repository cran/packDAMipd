% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_parameter_estimation_functions.R
\name{find_keyword_regression_method}
\alias{find_keyword_regression_method}
\title{Function to find the keyword for regression methods}
\usage{
find_keyword_regression_method(text, additional_info = NA)
}
\arguments{
\item{text}{regression method}

\item{additional_info}{additional information required}
}
\value{
the keyword that should be used in R for regression analysis
}
\description{
Function to find the keyword for regression methods
}
\details{
This function returns the keyword to use in regression methods.
For example linear regression requires lm in R
some regression methods require additional info and it has to be
provided
}
\examples{
find_keyword_regression_method("linear")
}
