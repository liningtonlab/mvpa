% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspection.R
\name{plot_correlation}
\alias{plot_correlation}
\title{Plot correlation results}
\usage{
plot_correlation(corr_list = NULL, rotate = FALSE, rel_font_size = 1)
}
\arguments{
\item{corr_list}{Result from the correlation() function.}

\item{rotate}{In case only one variable was correlated with the remaining, rotate the bar plot.}

\item{rel_font_size}{Relative font size based on default font size 11 (default 1).}
}
\value{
Either a squared color-coded correlation matrix or a bar plot.
}
\description{
This function uses the correlation results and plots them.
}
\examples{
corr_list <- correlation(HOMA_IR, response = "HOMA_IR")
plot_correlation(corr_list)

corr_list <- correlation(HOMA_IR, correlate = "variables")
plot_correlation(corr_list)
}
