% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversions.R
\name{dvar}
\alias{dvar}
\title{variance of d}
\usage{
dvar(d, n1, n2)
}
\arguments{
\item{d}{cohen's d estimate}

\item{n1}{cell size of group 1 for iv}

\item{n2}{cell size of group 2 for iv}
}
\description{
compute the sampling variance of d (for distinct groups, i.e., between-individual designs only)
}
\examples{
dvar(d = .5, n1 = 34, n2=26)

}
