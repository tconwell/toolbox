% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolbox.R
\name{namesToString}
\alias{namesToString}
\title{Pastes the names of a object into a string, optionally quoting the names.}
\usage{
namesToString(x, collapse = ",", quote = FALSE)
}
\arguments{
\item{x}{A named object (vector, list, data.frame)}

\item{collapse}{A string to separate the collapsed names.}

\item{quote}{TRUE/FALSE, if TRUE, adds quotes to the names.}
}
\value{
A string.
}
\description{
Pastes the names of a object into a string, optionally quoting the names.
}
\examples{
namesToString(c("test" = 1, "this" = 2))
}
