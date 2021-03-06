% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset.R
\name{subset_date}
\alias{subset_date}
\title{Subset by Date}
\usage{
subset_date(data = NULL, date = "date", freq = "week")
}
\arguments{
\item{data}{A dataset that includes a date variable.}

\item{date}{The name of your date variable in your data.}

\item{freq}{What frequency to subset to. E.g. "week", "month", etc.}
}
\value{
A tibble of data.
}
\description{
We sometimes need to subset high-frequency daily data to fit less high-frequency data that occurs on some other time interval. This function lets you pick the time range and does the subsetting for you.
}
\examples{
subset_date(data = pollbasepro, date = "date", freq = "week")
}
