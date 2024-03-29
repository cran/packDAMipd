% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/4b_probabilistic_sensitivity_analysis_functions.R
\name{summary_plot_psa}
\alias{summary_plot_psa}
\title{Function to summarise and plot probabilistic sensitivity analysis}
\usage{
summary_plot_psa(
  result_psa_params_control,
  result_psa_params_treat = NULL,
  threshold = NULL,
  comparator = NULL
)
}
\arguments{
\item{result_psa_params_control}{result from probabilistic
sensitivity analysis for first or control model}

\item{result_psa_params_treat}{result from probabilistic sensitivity
analysis for the comparative Markov model}

\item{threshold}{threshold value of WTP}

\item{comparator}{the strategy to be compared with}
}
\value{
plot of  sensitivity analysis
}
\description{
Function to summarise and plot probabilistic sensitivity analysis
}
\examples{
\donttest{
param_list <- define_parameters(
cost_direct_med_A = 1701,
cost_direct_med_B = 1774, tpAtoA = 0.2,
 tpAtoB = 0.5, tpAtoC = 0.3,
 tpBtoB = 0.3, tpBtoC = 0.7,
 tpCtoC = 1,cost_health_A = "cost_direct_med_A",
 cost_health_B = "cost_direct_med_B")
 sample_list <- define_parameters(cost_direct_med_A = "gamma(mean = 1701,
 sd = sqrt(1701))")
 A <- health_state("A", cost = "cost_health_A ", utility = 1)
 B <- health_state("B", cost = "cost_health_B", utility = 1)
 C <- health_state("C", cost = 0, utility = 0, absorb = "TRUE")
 tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
 colnames(tmat) <- rownames(tmat) <- c("A", "B", "C")
 tm <- populate_transition_matrix(3, tmat, c(
 "tpAtoA", "tpAtoB", "tpAtoC",  "tpBtoB", "tpBtoC", "tpCtoC"),
 colnames(tmat))
 health_states <- combine_state(A, B, C)
 mono_strategy <- strategy(tm, health_states, "mono")
 mono_markov <- markov_model(mono_strategy, 20, initial_state =c(1,0,0),
 discount = c(0.06, 0),param_list)
 param_table <- define_parameters_psa(param_list, sample_list)
 result <- do_psa(mono_markov, param_table, 3)
 result_plot <- summary_plot_psa(result, NULL, NULL, NULL)
 param_list_comb <- define_parameters(
 cost_direct_med_A = 1800, cost_direct_med_B = 1774, tpAtoA = 0.6,
 tpAtoB = 0.1, tpAtoC = 0.3,tpBtoB = 0.3, tpBtoC = 0.7,tpCtoC = 1,
 cost_health_A = "cost_direct_med_A",cost_health_B = "cost_direct_med_B")
 comb_strategy <- strategy(tm, health_states, "comb")
 comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
 discount = c(0.06, 0), param_list)
 param_table_comb <- define_parameters_psa(param_list_comb, sample_list)
 result_comb <- do_psa(comb_markov, param_table_comb, 3)
 summary_plot_psa(result, result_comb, 2000, "mono")
 }
}
