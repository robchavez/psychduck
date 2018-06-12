#' csus_mean_short Function
#'
#' This function calculates the scale mean for the short 18 items of the CSUS. Short scale items that corresponds to the full scale includes: Item# 1, 3, 7, 12, 14, 18, 19, 21, 24, 25, 27, 30, 32, 33, 34, 36, 38, 40.
#' @param data The dataframe.
#' @keywords mean
#' @export
#' @examples csus_mean_short(data)
#' csus_mean_short()

csus_mean_short <- function(data) {

  short = c("tom1", "tom3", "tom7", "tom12", "tom14", "tom18", "tom19", "tom21", "tom24", "tom25", "tom27", "tom30", "tom32", "tom33", "tom34", "tom36", "tom38", "tom40")
  data <- dplyr::mutate(data, csus_mean_short = rowMeans(data[ ,short], na.rm = TRUE))
  return(as.data.frame(data))

}
