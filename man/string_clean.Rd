% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_clean.R
\name{string_clean}
\alias{string_clean}
\title{Clean Characters}
\usage{
string_clean(
  x,
  keep_spaces = FALSE,
  keep_leading_num = FALSE,
  to_lower = FALSE,
  non_alnum_replace = NULL
)
}
\arguments{
\item{x}{character vector}

\item{keep_spaces}{logical indicating whether user wants to keep spaces by replacing them with "_"}

\item{keep_leading_num}{logical indicating whether you'd like to keep the leading numeric. It will be appended to the end of the clean string}

\item{to_lower}{logical indicating whether \code{x} should be lower case}

\item{non_alnum_replace}{optional string indicating the character to use to replace all non alpha numeric values except for spaces}
}
\value{
a character vector with all non-standard characters replaced by their standard counterparts
}
\description{
Clean Characters
}
\examples{
x <- "91 CesairÃ© T & co"
string_clean(x)

}
