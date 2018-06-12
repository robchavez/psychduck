#' A Standard Error Function
#'
#' This function allows you to calculate standard error.
#' @param x A single vector.
#' @keywords standard error
#' @export
#' @examples
#' SE(got$`Death Chapter`)

# Calculate standard error
SE <- function(x) {
  sqrt(var(x,na.rm=TRUE)/(length(na.omit(x))))
}
