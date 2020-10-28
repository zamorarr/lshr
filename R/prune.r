#' Prune results
#'
#' Apply jaccard similarity to candidates
#'
#' @param edges from \code{build_edges}
#' @param shingles from \code{shingle}
#'
#' @export
prune_edges <- function(edges, shingles, threshold = 0.8) {
  from <- edges$from
  to <- edges$to
  score <- purrr::map2_dbl(shingles[edges$from], shingles[edges$to], jaccard_shingles)

  # prune!
  from <- from[score >= threshold]
  to <- to[score >= threshold]
  list(from = from, to = to)
}

#' @export
build_edges <- function(results) {
  bucket_sizes <- vapply(results, length, integer(1L))
  #bucket_edges <- vapply(bucket_sizes, function(n) sum(seq_len(n - 1)), integer(1L))
  bucket_edges <- choose(bucket_sizes, 2)
  num_edges <- sum(bucket_edges)

  from <- integer(num_edges)
  to <- integer(num_edges)

  k <- 1L
  for (bucket in results) {
    len <- length(bucket)
    for (i in 1:(len - 1)) {
      for (j in (i+1L):len) {
        from[k] = bucket[i]
        to[k] = bucket[j]
        k <- k + 1L
      }
    }
  }

  list(from = from, to = to)
}

#' @export
prune_results <- function(results, shingles, threshold) {
  edges <- build_edges(results)
  prune_edges(edges, shingles, threshold)
}
