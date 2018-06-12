#' standard Error of Wide Format Data
#'
#' This function allows you to iteratively obtain standard error for each individual column in a wide format dataframe in which each row represents a particular subject and each column represents a specific test or task. This format generally displays summary data for particular tests or tasks where each individual cell is a mean or median of several trials of that particular test or task.
#' @param data The dataframe. Note that the dataframe should be in wide format where the first column represents individual subjects with only one row per subject and each additional column represents a specific test or task.
#' @param na.rm Default set to FALSE. If TRUE, missing data will be removed from the data frame before calculations proceed.
#' @keywords standard error, wide format, summary data
#' @export
#' @return dataframe
#' @examples subject_num <- c(1, 2, 3, 4) task1 <- c(99, 87, 82, 76) task2 <- c(67, 74, 68, 88) total <- as.data.frame(cbind(subject_num, task1, task2)) se_wide_format(total)
#' se_wide_format()



se_wide_format <- function(data, na.rm = FALSE) {
  se <- vector()
  test <- colnames(data)
  for (i in 2:ncol(data)) {
    if (na.rm)
      data <- data[!is.na(data)]
    x <- (sd(unlist(data[i])))/sqrt(nrow(data))
    se[i] <- x
  }
  output <- as.data.frame(cbind(test, se))
  output <- output[-1,]
}
