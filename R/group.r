#' @export
group_edges <- function(from, to) {
  result <- groupEdges(from, to)
  result$group <- dense_rank(result$group)
  result
}
