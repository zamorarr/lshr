#' Build edge data frame between documents
#'
#' Builds an edge list based on whether documents are similar based on their
#' shingles. Uses the candidates list to make comparing document much faster.
#'
#' @param candidates list of buckets with document ids from \code{lsh}
#' @param shingles list of documents and their shingles from \code{shingle}
#' @param threshold jaccard similarity threshold
build_edges <- function(candidates, shingles, threshold = 0.8) {
  edges <- lapply(candidates, function(bucket) {
    #for (bucket in results) {
    bucket <- sort(bucket) # to ensure a-b and b-a edges are always a-b
    len <- length(bucket)

    max_edges <- choose(len, 2)
    from <- integer(max_edges)
    to <- integer(max_edges)

    k <- 1L
    for (i in 1:(len - 1)) {
      for (j in (i + 1):len) {
        score <- jaccard_shingles(shingles[[bucket[i]]], shingles[[bucket[j]]])

        if (score >= threshold) {
          from[k] = bucket[i]
          to[k] = bucket[j]
          k <- k + 1L
        }
      }
    }

    # trim results
    from <- from[seq_len(k - 1)]
    to <- to[seq_len(k - 1)]
    tibble::tibble(from = from, to = to)
  })

  # combine bucket results
  edges <- do.call(rbind, edges)
  edges <- edges[!duplicated(edges),]
  edges
}

#' Group documents together into a cluster
#'
#' Similiar process to grouping vertices together that share edges into components
#'
#' @param edges data frame from \code{build_edges}
#' @param docs optional text data to include in results
group_edges <- function(edges, docs = NULL) {
  result <- groupEdges(edges$from, edges$to)
  result$group <- dense_rank(result$group)

  # order by group
  result <- tibble::as_tibble(result)
  result <- result[order(result$group, result$doc),]

  # add in docs
  if (!is.null(docs)) {
    result$text <- docs[result$doc]
  }
  # return result
  result
}

#' Tidy candidate results
#'
#' Uses \code{jaccard_shingles} to compare candidate pairs from \code{lsh} to determine
#' which documents are actually similar.
#'
#' @param candidates list of buckets with document ids from \code{lsh}
#' @param shingles list of documents and their shingles from \code{shingle}
#' @param docs optional text data to include in results
#' @param threshold jaccard similarity threshold
#' @export
tidy_candidates <- function(candidates, shingles, docs = NULL, threshold = 0.8) {
  edges <- build_edges(candidates, shingles, threshold)
  group_edges(edges, docs)
}
