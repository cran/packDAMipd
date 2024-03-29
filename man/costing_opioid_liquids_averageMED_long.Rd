% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/3c_costing_opioid_liquids_averageMED_functions.R
\name{costing_opioid_liquids_averageMED_long}
\alias{costing_opioid_liquids_averageMED_long}
\title{Function to estimate the cost of liquids when IPD is in long format}
\usage{
costing_opioid_liquids_averageMED_long(
  the_columns,
  ind_part_data_long,
  name_med,
  brand_med = NULL,
  bottle_size,
  bottle_size_unit = NULL,
  bottle_lasts,
  bottle_lasts_unit = NULL,
  preparation_dose,
  preparation_unit = NULL,
  timeperiod,
  unit_cost_data,
  unit_cost_column,
  cost_calculated_per,
  strength_column,
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = NULL,
  basis_strength_unit = NULL
)
}
\arguments{
\item{the_columns}{columns that are to be used to convert the data
from long to wide}

\item{ind_part_data_long}{IPD}

\item{name_med}{name of medication}

\item{brand_med}{brand name of medication if revealed}

\item{bottle_size}{size of the bottle used}

\item{bottle_size_unit}{unit of bottle volume}

\item{bottle_lasts}{how long the bottle lasted}

\item{bottle_lasts_unit}{time unit of how long the bottle lasted}

\item{preparation_dose}{dose if preparation is given}

\item{preparation_unit}{unit of preparatio dose}

\item{timeperiod}{time period for cost calculation}

\item{unit_cost_data}{unit costs data}

\item{unit_cost_column}{column name of unit cost in unit_cost_data}

\item{cost_calculated_per}{column name of unit where the cost is calculated}

\item{strength_column}{column column name that has strength of medication}

\item{list_of_code_names}{if names is coded, give the code:name pairs,
optional}

\item{list_of_code_brand}{if brand names  are coded, give the
code:brand pairs, optional}

\item{list_of_code_bottle_size_unit}{list of bottle size units and codes}

\item{list_of_code_bottle_lasts_unit}{list of time of bottle lasts and codes}

\item{list_preparation_dose_unit}{list of preparation dose units and codes}

\item{eqdose_covtab}{table to get the conversion factor for equivalent
doses, optional}

\item{basis_strength_unit}{strength unit to be taken as basis
required for total medication calculations}
}
\value{
the calculated cost of tablets along with original data
}
\description{
Function to estimate the cost of liquids when IPD is in long format
}
\examples{
med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_brand_empty.xlsx",
package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- costing_opioid_liquids_averageMED_wide(
ind_part_data = ind_part_data, name_med = "liq_name",
brand_med =  "liq_brand", bottle_size = "liq_bottle_size",
bottle_size_unit = NULL, bottle_lasts = "liq_lasts",
bottle_lasts_unit = NULL, preparation_dose = "liq_strength",
preparation_unit = NULL, timeperiod = "1 day",
unit_cost_data = med_costs, unit_cost_column = "UnitCost",
cost_calculated_per = "Basis", strength_column = "Strength",
list_of_code_names = NULL, list_of_code_brand = NULL,
list_of_code_bottle_size_unit = NULL,
list_of_code_bottle_lasts_unit = NULL,
list_preparation_dose_unit = NULL, eqdose_covtab = table,
basis_strength_unit = NULL)
}
