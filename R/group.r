#' Group edges into graph components
#'
#' @param from,to indexes of nodes
#' @export
group_edges <- function(from, to) {
  stopifnot(identical(length(from), length(to)))

  # find all nodes
  nodes <- sort(unique(c(from, to)))

  # make sure nodes are sequential
  n <- length(nodes)
  if (!identical(as.integer(max(nodes)), n)) {
    stop("node ids must be sequential starting from 1", call. = FALSE)
  }

  # initialize each node to its own group
  node_group <- seq_len(n)
  groups <- lapply(node_group, function(i) i)

  # loop through edges, adding nodes to the same group
  for (i in seq_along(from)) {
    # merge node groups together
    r <- merge_groups(groups, node_group, from[i], to[i])
    groups <- r$groups
    node_group <- r$node_group
  }

  # return groups
  dense_rank(node_group)
}

merge_groups <- function(groups, node_group, from, to) {
  # exit if groups are the same
  from_group <- node_group[from]
  to_group <- node_group[to]
  if (identical(from_group, to_group)) return(list(groups = groups, node_group = node_group))

  # otherwise move nodes from to_group to-from_group
  from_nodes <- groups[[from_group]]
  to_nodes <- groups[[to_group]]

  groups[[from_group]] <- c(from_nodes, to_nodes) # this is the culprit
  groups[[to_group]] <- NA_integer_

  node_group[to_nodes] <- from_group
  #node_group[to_group] <- from_group

  # return results
  return(list(groups = groups, node_group = node_group))
}

#' @export
group_edges_rcpp <- function(from, to) {
  result <- groupEdges(from, to)
  result$group <- dense_rank(result$group)
  result
}
