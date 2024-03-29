% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_parameter_estimation_survival.R
\name{plot_return_residual_survival}
\alias{plot_return_residual_survival}
\title{Plotting and return the residuals after survival model}
\usage{
plot_return_residual_survival(
  param_to_be_estimated,
  indep_var,
  covariates,
  fit
)
}
\arguments{
\item{param_to_be_estimated}{parameter to be estimated}

\item{indep_var}{independent variable}

\item{covariates}{covariates}

\item{fit}{fit object from survreg method}
}
\value{
plot and the residuals
}
\description{
Plotting and return the residuals after survival model
}
\examples{
\donttest{
data_for_survival <- survival::lung
surv_estimated <- use_parametric_survival("status", data_for_survival,
"sex",
info_distribution = "weibull",covariates = c("ph.ecog"), "time")
plot_return_residual_survival("status", "sex",
covariates = c("ph.ecog"),surv_estimated$fit)
}
}
