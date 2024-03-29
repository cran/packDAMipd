% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_parameter_estimation_functions.R
\name{do_diagnostic_linear_regression}
\alias{do_diagnostic_linear_regression}
\title{Do the diagnostic test for lm model assumption}
\usage{
do_diagnostic_linear_regression(
  method,
  fit,
  expression_recreated,
  param_to_be_estimated,
  dataset,
  indep_var,
  covariates,
  interaction
)
}
\arguments{
\item{method}{param describing the methods, expects lm}

\item{expression_recreated}{the expression recreated for calling lm}

\item{param_to_be_estimated}{parameter of interest}

\item{dataset}{data set to be provided}

\item{indep_var}{the independent variable (column name in data file)}

\item{covariates}{list of covariates - calculations to be done before
passing}

\item{interaction}{boolean value to indicate interaction in the case of
linear regression, false by default}
}
\value{
the results of the regression analysis
}
\description{
Do the diagnostic test for lm model assumption
}
\examples{
\donttest{
datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
mydata <- read.csv(datafile)
results_logit <- use_linear_regression("admit",dataset = mydata,
indep_var = "gre",covariates = NA, interaction = FALSE)
do_diagnostic_linear_regression("lm", results_logit$fit,
results_logit$fit$call,
"admit", mydata, "gre", covariates = NA , interaction= FALSE)
}
}
\keyword{internal}
