% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_items.R
\name{name_items}
\alias{name_items}
\title{Questionnaire naming function}
\usage{
name_items(x, name = "item", new_df = "new_df")
}
\arguments{
\item{x}{The vector with all the columns to be labeled. All columns must be numeric.}

\item{name}{The name you want to assign to the columns; (default = "item"); must be a string. For each column, the function will add a seperator (_) and a number to this name.}

\item{new_df}{The name of the new dataframe where x will be saved, after the column names are changed; (default = "new_df"); must be string.}
}
\description{
This function allows you to serially name large numbers of questionnaire items or columns (e.g., item_1, item_2,...,item_113)
}
\examples{
#example1
data("mtcars") #read in the mtcars dataset
head(mtcars) #have a look at the variable names
name_items(mtcars) #run the function on mtcars
head(new_df) #see how the variable names changed in the new dataframe
example2
library(psych) #load the psych package
data("bfi") #read in the bfi dataset from the psych package
test <- as.data.frame(cbind(bfi$A1,bfi$A2,bfi$A3,bfi$A4,bfi$A5)) #save the 5 agreeableness items as a dataframe
head(test) #have a look at the variable names
name_items(test, "agree", "agreeableness") #run the function to name the columns
head(agreeableness) #see how the variable names changed in the new dataframe
}
\keyword{column}
\keyword{label}
\keyword{name}
\keyword{questionnaire}
