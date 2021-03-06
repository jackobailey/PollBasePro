% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/party_colours.R
\name{party_colours}
\alias{party_colours}
\title{British Political Party Colours}
\usage{
party_colours(x)
}
\arguments{
\item{x}{A vector of party names.}
}
\value{
A vector of corresponding hex codes
}
\description{
This function takes a vector of party names and returns the parties' corresponding colours as hex values. It includes a few under-the-hood tricks to match on all manner of different names.
}
\examples{
party_colours(c("Lab", "lab", "LD", "Tories", "ScotNat", "SNP", "grn"))
}
