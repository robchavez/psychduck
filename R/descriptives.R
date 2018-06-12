#' A Descriptives Function
#'
#' This function allows you to examine your favorite descriptive statistics.
#' @param data The dataframe. Note that all variables must be numeric.
#' @param si.digits The number of digits to round to. Defaults to 3 decimal places.
#' @keywords descriptive stats, valid vases, missing cases, mean, standard deviation, standard error, skew, kurtosis
#' @export
#' @examples library(car)
#' @examples #Load Prestige of Canadian Occupations Data from car package:
#' @examples data <- Prestige
#' @examples head(data)
#' @examples str(data)
#' @examples #Prepare data--all variables must be numeric
#' @examples data <- colfacttonum(data)
#' @examples data <- colinttonum(data)
#' @examples descriptives(data)
#' descriptives()

descriptives <- function(data,si.digits=3) {
  #require(psych)
  temp1 <- data
  output <- matrix(rep(NA,ncol(temp1)*7),ncol = 7)
  for (i in 1:ncol(temp1)) {
    output[i,1] <- length(na.omit(temp1[,i]))
    output[i,2] <- sum(is.na(temp1[,i]))
    output[i,3] <- round(mean(temp1[,i], na.rm = TRUE),si.digits)
    output[i,4] <- round(sd(temp1[,i], na.rm = TRUE),si.digits)
    output[i,5] <- round(sd(temp1[,i], na.rm = TRUE)/(sqrt(output[i,1])),si.digits)
    output[i,6] <- round(skew(temp1[,i], na.rm = TRUE),si.digits)
    output[i,7] <- round(kurtosi(temp1[,i], na.rm = TRUE),si.digits)
  }
  output <- data.frame(output)
  colnames(output) <- c("n", "nmiss", "mean", "std", "se", "skew", "kurtosis")
  rownames(output) <- colnames(temp1)
  return(output)
}
