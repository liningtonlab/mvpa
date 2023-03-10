% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_manipulation.R
\name{add_unique_ids}
\alias{add_unique_ids}
\title{Construct unique IDs for rownames.}
\usage{
add_unique_ids(data = NULL)
}
\arguments{
\item{data}{Dataframe.}
}
\value{
Dataframe with new row names or a new id column.
}
\description{
Substitutes current row indices with unique IDs based on row index value and leading zeros.
}
\examples{
# df <- add_unique_id(data = df)
}
