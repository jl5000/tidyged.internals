% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{date_exact}
\alias{date_exact}
\title{Construct a DATE_EXACT string}
\usage{
date_exact(year = integer(), month = integer(), day = integer())
}
\arguments{
\item{year}{The year.}

\item{month}{The month of the year.}

\item{day}{The day of the month.}
}
\value{
A DATE_EXACT string
}
\description{
Construct a DATE_EXACT string
}
\details{
Even though the day, month, and year are all required for an exact date, empty default
values have been set so that they are represented as zero-length rows in the tidyged file (i.e.
omitted).
}
