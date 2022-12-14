% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolbox.R
\name{jsonStr}
\alias{jsonStr}
\title{Format data as a JSON object (like this: {"x": "120"}).}
\usage{
jsonStr(name, val)
}
\arguments{
\item{name}{A string, the name of the JSON entry}

\item{val}{A string, the value to associate with the JSON entry.}
}
\value{
A string, data formatted as a JSON object.
}
\description{
Format data as a JSON object (like this: {"x": "120"}).
}
\examples{
jsonStr(name = "var1", val = "Blue")
}
