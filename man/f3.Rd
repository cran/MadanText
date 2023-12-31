% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/madantext.r
\name{f3}
\alias{f3}
\title{Persian Text Normalization and Stemming}
\usage{
f3(x)
}
\arguments{
\item{x}{A character vector of Persian text.}
}
\value{
Returns a character vector where each element is the normalized
and stemmed version of the corresponding element in the input vector.
Specifically, it performs character replacement and stemming on each
element of the input, thereby returning a vector of the same length
but with processed text. If an element cannot be processed, it will be
returned as NA in the output vector.
}
\description{
This function normalizes Persian text by replacing specific characters
and applies stemming.
}
\examples{
\dontrun{
  text <- c("Persian text here")
  normalized_text <- f3(text)
}
}
