% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pls_mc_sr.R
\name{plot_cost_function_values_distribution}
\alias{plot_cost_function_values_distribution}
\title{Plot cost function values distribution}
\usage{
plot_cost_function_values_distribution(
  result_list = NULL,
  show_title = TRUE,
  rel_font_size = 1
)
}
\arguments{
\item{result_list}{Result list obtained from \code{perform_mc_pls_tp()}.}

\item{show_title}{Do not show title}

\item{rel_font_size}{Relative font size based on default font size 11 (default 1).}
}
\value{
Points plot with cost function values and segments indicating the median cost function value of the lesser component.
}
\description{
Plots the median of the cost function values (eg. RMSEP) of the lower component n
next to the respective cost function values of the next higher component n+1. The ratio of values falling
below the median versus the total count of performed repetitions result in the validation threshold.
This plot complements the \code{plot_cost_function()} function.
}
\examples{
# # result_list obtained from perform_mc_pls_tp()
# plot_cost_function_vals_distribution(result_list)
}
