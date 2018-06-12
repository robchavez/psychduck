#' A Colinttonum Function
#'
#' This function changes all columns that are integers to numeric.
#' @param data The dataframe.
#' @keywords data structure, data class, integer, numeric
#' @export
#' @examples library(car)
#' @examples #Load Prestige of Canadian Occupations Data from car package:
#' @examples data <- Prestige 
#' @examples data$income <- as.character(data$income) 
#' @examples head(data)
#' @examples str(data)
#' @examples data <- colinttonum(data)
#' @examples #Re-run str(data) to make sure all integers were converted to numeric--should have changed one variable
#' @examples str(data)
#' colinttonum()

colinttonum <- function(data) {
  temp1 <- data
  temp2 <- names(which(unlist(lapply(temp1[,1:ncol(temp1)],class)) == "integer"))
  for (i in 1:length(temp2)) {
    temp1[,temp2[i]] <- as.numeric(temp1[,temp2[i]])
  }
  warning(paste(length(temp2)," of ",ncol(temp1)," total columns were converted to numeric.",sep = ""))
  return(temp1)
}
