% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohen_d.R
\name{cohen_d}
\alias{cohen_d}
\title{Effect Size for Repeated Measures Using Cohen's D}
\usage{
cohen_d(data, col1, col2, na.rm = FALSE)
}
\arguments{
\item{data}{A dataframe for a repeated measures design}

\item{col1}{The name of the column that represents a paticular condition}

\item{col2}{The name of the repeated measure that is being compared to the first column that was identify}

\item{na.rm}{Default set to FALSE. If TRUE, missing data will be removed from the data frame before calculations proceed.}
}
\description{
This function allows you to calculate Cohen's D effect size for a repeated measures design.
}
\examples{
neutral <- c(.92, .94, .84, .82, .93, .76, .78, .84, .97, .98) emotion <- c(.86, .78, .45, .75, .87, .65, .63, .76, .87, .91) total <- as.data.frame(cbind(neutral, emotion)) cohen_d(total, col1 = "neutral", col2 = "emotion")
se_wide_format()
}
\keyword{cohen's}
\keyword{d,}
\keyword{effect}
\keyword{measures}
\keyword{repeated}
\keyword{size,}
