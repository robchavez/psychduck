#' A Blue Plot Function
#'
#' This function allows you to create a plot in blue.
#' @param x A single vector.
#' @param y A single vector.
#' @keywords blue plot
#' @export
#' @examples
#' plot_blue(got$`Book Intro Chapter`, got$`Death Chapter`)

# Create a plot in blue 
plot_blue <- function(x,y) {
  plot(x,y,col="blue")
}