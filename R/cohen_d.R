#' Effect Size for Repeated Measures Using Cohen's D
#'
#' This function allows you to calculate Cohen's D effect size for a repeated measures design.
#' @param data A dataframe for a repeated measures design
#' @param col1 The name of the column that represents a paticular condition
#' @param col2 The name of the repeated measure that is being compared to the first column that was identify
#' @param na.rm Default set to FALSE. If TRUE, missing data will be removed from the data frame before calculations proceed.
#' @keywords effect size, cohen's d, repeated measures
#' @export
#' @examples neutral <- c(.92, .94, .84, .82, .93, .76, .78, .84, .97, .98) emotion <- c(.86, .78, .45, .75, .87, .65, .63, .76, .87, .91) total <- as.data.frame(cbind(neutral, emotion)) cohen_d(total, col1 = "neutral", col2 = "emotion")
#' se_wide_format()


cohen_d <- function(data, col1, col2, na.rm = FALSE) {
  if (na.rm) {
    data <- data[!is.na(data)]
  }
  pooled <- sqrt(((sd(data[[col1]])^2) + (sd(data[[col2]])^2))/2)
  (mean(data[[col1]]) - mean(data[[col2]]))/pooled
}
