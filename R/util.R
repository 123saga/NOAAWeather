#' @title remove whitespace
#' @description remove whitespaces from any data
#' @param x input data
#' @return data without whitespaces
trim <- function (x) {
  # to avoid scentific notation
  options(scipen = 999)
  gsub("^\\s+|\\s+$", "", x)
}
