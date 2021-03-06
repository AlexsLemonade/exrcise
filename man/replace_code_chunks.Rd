% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_code_chunks.R
\name{replace_code_chunks}
\alias{replace_code_chunks}
\title{Replace code from an rmarkdown document as a vector of lines}
\usage{
replace_code_chunks(lines, replace_flags, replacement = "", comment = TRUE)
}
\arguments{
\item{lines}{A character vector of lines from an Rmarkdown document
which will be parsed and have included code chunks selectively replaced.}

\item{replace_flags}{A character vector of code chunk options
that if set to TRUE will have their code replaced.}

\item{replacement}{A character vector that code chunks will be
replaced with, one element per line.}

\item{comment}{TRUE or FALSE; whether to include comments.
Default is TRUE}
}
\value{
A vector of lines that forms the content of a new .Rmd document
}
\description{
Replace code from an rmarkdown document as a vector of lines
}
\examples{
\dontrun{
replace_code_chunks(lines,
    replace_flags = "solution",
    replacement = "### Your Code Here")
}
}
