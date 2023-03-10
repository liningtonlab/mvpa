% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.R
\name{plot_variable_variation_pca}
\alias{plot_variable_variation_pca}
\title{Plot variable variation}
\usage{
plot_variable_variation_pca(
  pca_result = NULL,
  x_filter = NULL,
  select_PCs = 5,
  rotate = FALSE,
  rel_font_size = 1
)
}
\arguments{
\item{pca_result}{PCA result obtained from perform_pca() method.}

\item{x_filter}{Filter for variables / features.}

\item{select_PCs}{Select principal components that will be shown in detail. Residual PCs will be summarized in a residual value. If NULL, all PCs will be shown (default is 5).}

\item{rotate}{Rotate plot by 90 degrees to read axis labels more easily.}

\item{rel_font_size}{Relative font size (default 1, font size 11).}
}
\value{
Variable variation plot
}
\description{
The function creates a stacked bar plot where each bar represents a variable and
each individual section of one bar represents the explained variation
by the individual principal component.
}
\examples{

# pca_result <- perform_pca(HOMA_IR)
# plot_variable_variation_pca(var_variation, select_PCs = c("PC1", "PC3"))
}
