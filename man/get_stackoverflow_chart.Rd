% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stackoverflow.R
\name{get_stackoverflow_chart}
\alias{get_stackoverflow_chart}
\title{Chart StackOverflow Answer Activity}
\usage{
get_stackoverflow_chart(user = NULL, addAccepted = TRUE, showPlot = TRUE,
  max.pages = 10)
}
\arguments{
\item{user}{StackOverflow user id}

\item{addAccepted}{logical. Should the number of accepeted answers be 
overlayed?}

\item{showPlot}{logical. Should the plot be displayed? Otherwise 
\code{ggplot2} object is just returned.}

\item{max.pages}{how many pages of results (100 per page) should be 
traversed?}
}
\value{
a list containing the ggplot object and data object:
  
  list(plot = <ggplot2 object>, data = <data.frame>)
}
\details{
As per the API 
  \url{https://api.stackexchange.com/docs/answers-on-users}, returns a
  \code{data.frame} of columns described in
  \url{https://api.stackexchange.com/docs/types/answer} for the requested \code{user}.
  
  This function utilises both \code{\link[githubtools]{theme_github}} and 
  \code{\link[githubtools]{scale_fill_social}} and is subject to updates to 
  those functions.
}
\examples{
\dontrun{
library(butteRfly)
get_stackoverflow_chart(user = 4168169) # @Jonathan Carroll
get_stackoverflow_chart(user = 496803)  # @thelatemail }
}

