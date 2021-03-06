% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{gedcom_value_update}
\alias{gedcom_value_update}
\title{Update particular values in a tidyged object}
\usage{
gedcom_value_update(
  gedcom,
  record_xref,
  tag,
  level,
  old_value,
  new_value,
  after_tag = NULL,
  after_value = NULL
)
}
\arguments{
\item{gedcom}{A tidyged object.}

\item{record_xref}{The xref of the record in which to update the value.}

\item{tag}{The tag associated with the value.}

\item{level}{The level number of the value.}

\item{old_value}{The old value.}

\item{new_value}{The new value.}

\item{after_tag}{Whether the tag should be subordinate to this parent tag.}

\item{after_value}{Whether the value should be subordinate to this parent value.}
}
\value{
A tidyged object with the value updated.
}
\description{
Update particular values in a tidyged object
}
