% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_parameter_estimation_functions.R
\name{find_keyword_rand_generation}
\alias{find_keyword_rand_generation}
\title{Function to find the keyword for generating random numbers the  distribution}
\usage{
find_keyword_rand_generation(text)
}
\arguments{
\item{text}{name of the probability distribution}
}
\value{
the keyword that should be used in R for generating random numbers
}
\description{
Function to find the keyword for generating random numbers the  distribution
}
\details{
This function returns the keyword for generating random number using
a keyword provided that is generally used for prob distribution
(but R might require a different keyword)
}
\examples{
find_keyword_rand_generation("gamma")
}
