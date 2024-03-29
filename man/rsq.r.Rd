% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversions.R
\name{rsq.r}
\alias{rsq.r}
\title{r squared to r}
\usage{
rsq.r(rsq, dir)
}
\arguments{
\item{rsq}{delta r squared from regression}

\item{dir}{r squared is in absolute value, so please supply the empirical direction of the link (+1 or -1)}
}
\description{
compute the semi-partial correlation coefficient from delta r squared
}
\examples{
# a paper reports delta r squared as .04 and in text describes a negative association
rsq.r(.04, -1)

}
