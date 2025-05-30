% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_data.R
\name{prep_data}
\alias{prep_data}
\title{Read in and join all required tables}
\usage{
prep_data(db)
}
\arguments{
\item{db}{a list of tables produced by \code{\link[=read_fia]{read_fia()}}}
}
\value{
a tibble
}
\description{
Reads in all the tables needed for carbon estimation and population scaling
and joins them into a single table. Then, some additional wrangling steps are
performed.
\enumerate{
\item Creates unique tree an plot identifiers (\code{tree_ID} and \code{plot_ID},
respectively).
\item Filters out intensification plots.
\item Removes trees that were measured in error (\code{RECONCILECD} 7 or 8).
\item Removes trees with no measurements because they've always been fallen
(\code{STANDING_DEAD_CD} 0).
\item Fills in missing values for \code{ACTUALHT} with values from \code{HT} to prepare
for interpolation.
\item Overwrites \code{SPCD} with whatever the last value of \code{SPCD} is for each tree
(to handle trees that change \code{SPCD})
}
}
