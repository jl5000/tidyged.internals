% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subrecords.R
\name{FAMILY_EVENT_DETAIL}
\alias{FAMILY_EVENT_DETAIL}
\title{Construct the FAMILY_EVENT_DETAIL tibble}
\usage{
FAMILY_EVENT_DETAIL(
  husband_age_at_event = character(),
  wife_age_at_event = character(),
  event_details = EVENT_DETAIL()
)
}
\arguments{
\item{husband_age_at_event}{A string that indicates the age in years, months, and days that the
husband was at the time of the associated event. See Details.}

\item{wife_age_at_event}{A string that indicates the age in years, months, and days that the
wife was at the time of the associated event. See Details.}

\item{event_details}{An EVENT_DETAIL() object giving details of the event.}
}
\value{
A tidy tibble containing the FAMILY_EVENT_DETAIL part of a GEDCOM file.
}
\description{
This function constructs a tibble representation of the FAMILY_EVENT_DETAIL from the GEDCOM 5.5.5
specification.
}
\details{
For ages, any labels must come after their corresponding number, for example; 4y 8m 10d.
The line value should be normalised; it should for example not specify 2y 13m, but 3y 1m
instead. Number of days is allowed to be 365 because of leap years.
The YYY, MM and DDD values must not be zero; if a value equals zero, that part is left off.
The values may not contain leading zeroes either.
}
