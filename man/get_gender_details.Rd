% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3a_trialdata_analysis_input_functions.R
\name{get_gender_details}
\alias{get_gender_details}
\title{Function to get the details of the gender column}
\usage{
get_gender_details(trialdata)
}
\arguments{
\item{trialdata, }{data containing individual level trial data}
}
\value{
the name of the variable related to gender and the unique contents
if success, else error
}
\description{
Function to get the details of the gender column
}
\details{
expecting the data contains the information on gender
preferably  column names "gender", "sex" or "male" or "female".
If multiple column names match these, then first match will be chosen.
}
\examples{
get_gender_details(data.frame("Age" = c(21, 15), "sex" = c("m", "f")))
}
