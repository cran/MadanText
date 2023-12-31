% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/madantext.r
\name{f7}
\alias{f7}
\title{Extract and Count Specific Parts of Speech}
\usage{
f7(UPIP, type)
}
\arguments{
\item{UPIP}{A data frame with columns 'upos' (POS tags) and 'lemma' (lemmatized tokens).}

\item{type}{A string representing the POS to filter (e.g., 'NOUN', 'VERB').}
}
\value{
Returns a data frame where each row corresponds to a unique lemma
of the specified POS type. The data frame has two columns: 'key',
which contains the lemma, and 'freq', which contains the frequency
count of that lemma in the data. The rows are ordered in decreasing
frequency of occurrence. This format is useful for quickly
identifying the most common terms of a particular POS in the data.
}
\description{
This function extracts tokens of a specified part of speech (POS)
from the given data frame and counts their frequency.
}
\examples{
data <- data.frame(upos = c('NOUN', 'VERB'), lemma = c('house', 'run'))
noun_freq <- f7(data, 'NOUN')
}
