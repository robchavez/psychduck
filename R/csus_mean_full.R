#' csus_mean_full Function
#'
#' This function calculates the scale mean for the full 42 items of the CSUS
#' @param data The dataframe.
#' @keywords mean
#' @export
#' @examples csus_mean_full(data)
#' csus_mean_full()

csus_mean_full <- function(data) {

  full = c(paste0("tom", seq(1:42)))
  data <- dplyr::mutate(data, csus_mean_full = rowMeans(data[ ,full], na.rm = TRUE))
  return(as.data.frame(data))

}
