#' csus_recode Function
#'
#' This function recodes items in the CSUS - Items #5 12, 25, 35, 39, 41 - that needs to be reverse coded. The csus_recode function will recode into the same variable as opposed to creating a new variable.
#' @param data The dataframe.
#' @keywords recode
#' @export
#' @examples csus_recode(data)
#' csus_recode()

csus_recode <- function(data) {
  print(data$tom5, quote = F)
  cols = c("tom5", "tom12", "tom25", "tom35", "tom39", "tom41")
  data[ ,cols] = 5 - data[ ,cols]
  print(data$tom5, quote = F)
  warning(paste("csus item 5 is printed as comparison before (top) and after (bottom) recoding", sep = ""))
}
