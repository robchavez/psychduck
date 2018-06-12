#' Importing list of Matlab files
#'
#' This function allows you to import a list of specific .mat files in your working directory by specifying a particular pattern that is in the matlab file name.
#' @param pattern A pattern that is unique to a set of matlab files in your working directory in the form of a string
#' @keywords .mat, matlab, pattern, list, import
#' @export
#' @return list of files
#' @examples mat_file_import("_test1_")


mat_file_import <- function(pattern) {
  files <- list.files(pattern = "\\.mat")
  files <- files[grep(pattern, files)]
  map(files, rmatio::read.mat)
}
