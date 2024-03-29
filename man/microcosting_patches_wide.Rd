% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3c_costing_medication_patches_functions.R
\name{microcosting_patches_wide}
\alias{microcosting_patches_wide}
\title{Function to estimate the cost of patches taken (from IPD)}
\usage{
microcosting_patches_wide(
  ind_part_data,
  name_med,
  brand_med = NULL,
  dose_med,
  unit_med = NULL,
  no_taken,
  freq_taken,
  timeperiod,
  unit_cost_data,
  unit_cost_column,
  cost_calculated_per,
  strength_column,
  list_of_code_names = NULL,
  list_of_code_freq = NULL,
  list_of_code_dose_unit = NULL,
  list_of_code_brand = NULL,
  eqdose_cov_tab = NULL,
  basis_strength_unit = NULL
)
}
\arguments{
\item{ind_part_data}{IPD}

\item{name_med}{name of medication}

\item{brand_med}{brand name of medication if revealed}

\item{dose_med}{dose of medication used}

\item{unit_med}{unit of medication ; use null if its along with the dose}

\item{no_taken}{how many taken}

\item{freq_taken}{frequency of medication}

\item{timeperiod}{time period for cost calculation}

\item{unit_cost_data}{unit costs data}

\item{unit_cost_column}{column name of unit cost in unit_cost_data}

\item{cost_calculated_per}{column name of unit where the cost is calculated}

\item{strength_column}{column column name that contain strength of
medication}

\item{list_of_code_names}{if names is coded, give the code:name pairs,
optional}

\item{list_of_code_freq}{if frequency is coded, give the
code:frequency pairs, optional}

\item{list_of_code_dose_unit}{if unit is coded, give the code:unit pairs,
optional}

\item{list_of_code_brand}{if brand names  are coded, give the code:brand
pairs, optional}

\item{eqdose_cov_tab}{table to get the conversion factor for equivalent
doses, optional, but the column names have to unique
Similar to c("Drug",    "form", "unit",    "factor") or
c("Drug",    "form", "unit",    "conversion")}

\item{basis_strength_unit}{strength unit to be taken as basis
required for total medication calculations}
}
\value{
the calculated cost of tablets along with original data
}
\description{
Function to estimate the cost of patches taken (from IPD)
}
\details{
Assumes individual level data has name of medication, dose, dose unit,
number taken, frequency taken, and basis time
Assumes unit cost data contains the name of medication, form/type,
strength, unit of strength (or the unit in which the cost calculated),
preparation, unit cost, size and size unit
(in which name, forms, size, size unit, and preparation  are not passed on)
@importFrom dplyr \%>\%
a patient use 1 mg/hr patches 5 patches once a week
that patch comes in a pack of 4 with cost £2.50
we want to estimate the cost for 3 months
that means amount of medication
3 months = 21 weeks
number of patches taken = 21\emph{5 = 105 patches
packs = (105/4) almost 27 packs
cost = 27}2.50
}
\examples{
med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
package = "packDAMipd")
data_file <- system.file("extdata", "medication.xlsx",
package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx", package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_patches_wide(
ind_part_data = ind_part_data, name_med = "patch_name",
brand_med = "patch_brand", dose_med = "patch_strength", unit_med = NULL,
no_taken = "patch_no_taken", freq_taken = "patch_frequency",
timeperiod = "4 months", unit_cost_data = med_costs,
unit_cost_column = "UnitCost", cost_calculated_per  = "Basis",
strength_column = "Strength", list_of_code_names = NULL,
list_of_code_freq = NULL, list_of_code_dose_unit = NULL,
list_of_code_brand = NULL, eqdose_cov_tab = table,
basis_strength_unit = "mcg/hr")
}
