% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolbox.R
\name{doubleQuoteText}
\alias{doubleQuoteText}
\title{Add double quotes to strings.}
\usage{
doubleQuoteText(
  x,
  char_only = TRUE,
  excluded_chars = c("NULL"),
  null_or_na_as_NULL = TRUE
)
}
\arguments{
\item{x}{A string.}

\item{char_only}{TRUE/FALSE, if TRUE, adds quotes only if is.character(x) is TRUE.}

\item{excluded_chars}{A character vector, will not add quotes if a value is in excluded_chars.}

\item{null_or_na_as_NULL}{TRUE/FALSE, if TRUE, NULL and NA values are replaced with the string "NULL".}
}
\value{
A string, with double quotes added.
}
\description{
Add double quotes to strings.
}
\examples{
doubleQuoteText("Sample quotes.")
}
