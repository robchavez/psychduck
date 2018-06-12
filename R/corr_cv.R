#' A Cross-Validated Correlation Function
#'
#' This function allows you to calculate cross-validated correlations.
#' @param x A single vector.
#' @param y A single vector.
#' @keywords cross-validation correlation
#' @export
#' @examples
#' corr_cv(got$`Book Intro Chapter`, got$`Death Chapter`)

# Cross-validated correlation
corr_cv <- function(x, y) {
  corval <- vector()
  for (i in 1:length(x)) {
    corval[i] <- cor(x[-i], y[-i], use = "pairwise.complete.obs")
  }
  mean_corr <- mean(corval)
  return(mean_corr)
}
