% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringsplit.R
\name{stringsplit}
\alias{stringsplit}
\title{Split a string into string vector}
\usage{
stringsplit(in_str, splt = " ")
}
\arguments{
\item{in_str}{Input string to be split}

\item{splt}{Character to split at. By default, split at spaces.}
}
\value{
Returns string vector or character vector from strings.
}
\description{
`stringsplit` is an extension of `base::strsplit` but returns a vector
instead of a list. This function also removes blank characters and spaces.
}
\examples{
# str_split("Hello World")
## c("Hello", "World")

# str_split("Hello World")
## c("H", "e", "l", "l", "o", "W", "o", "r", "l", "d")
}
