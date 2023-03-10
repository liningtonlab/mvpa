% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_manipulation.R
\name{keep_objects}
\alias{keep_objects}
\title{Keep selected objects}
\usage{
keep_objects(data = NULL, objs_to_keep = NULL)
}
\arguments{
\item{data}{Dataframe.}

\item{objs_to_keep}{Object name(s) that shall be kept in the dataframe. The rest will be removed.}
}
\value{
Dataframe with kept objects only.
}
\description{
Selected objectss are kept while the others are removed.
}
\examples{
# df <- add_unique_ids(df)
# df <- keep_objects(df, c("0011", "0231", "2357"))
# df <- keep_object(df, "0042")
}
