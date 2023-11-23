\name{stats}
\alias{stats}
\title{Statistics and Plots}
\description{
  This function creates a histogram and a boxplot of the given numeric vector
  and returns basic statistics including minimum, median, mean, maximum, standard deviation, and range.
}
\usage{
stats(x)
}
\arguments{
  \item{x}{A numeric vector for which statistics and plots will be generated.}
}
\value{
  A data frame containing the minimum, median, mean, maximum, standard deviation, and range of \code{x}.
}
\examples{
data_vector <- rnorm(100)
stats(data_vector)
}
