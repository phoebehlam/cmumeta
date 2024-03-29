% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanebsco.R
\name{cleanebsco}
\alias{cleanebsco}
\title{from ebsco psycinfo output to cleaned screening sheet}
\usage{
cleanebsco(path, name, windows = F)
}
\arguments{
\item{path}{this is the path of the folder containing the .xlsx file converted from the xml EBSCO file.}

\item{name}{this is the name of the .xlsx file converted from the xml EBSCO file.}
}
\description{
This function turns the .xlsx file (converted from the EBSCO xml file) into a clean xlsx that provides article ID, first author, year of publication, journal, and title.
}
\examples{
cleanebsco(path = '/Users/phoebelam/Desktop/problemset', name = 'ebsco export').
}
