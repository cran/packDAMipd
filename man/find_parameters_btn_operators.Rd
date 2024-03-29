% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_model_functions.R
\name{find_parameters_btn_operators}
\alias{find_parameters_btn_operators}
\title{Function to return parameters with in a expression containing operators}
\usage{
find_parameters_btn_operators(expr)
}
\arguments{
\item{expr}{an expression}
}
\value{
parameters in the expression expr
}
\description{
Function to return parameters with in a expression containing operators
}
\details{
This function returns the parameters between the operators
if the state value or probabilities are defined as expressions, we need
to extract the parameters and then assign
First the position of all operators are found and then return the parameters
separated by those operators
This happens only for one level
find_parameters_btn_operators("a+b") provides a and b
but  for find_parameters_btn_operators("mean(a,b)+b")
provides mean(a,b) and b
}
\examples{
find_parameters_btn_operators("a+b")
}
