#' Dense rank
#'
#' Taken from dplyr
#' @param x vector of values to rank
dense_rank <- function(x) fastmatch::fmatch(x, sort(unique(x)))
