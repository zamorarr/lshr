#' @export
build_edges <- function(results, shingles, threshold = 0.8) {
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
      for (j in (i + 1):len) {
        score <- jaccard_shingles(shingles[[bucket[i]]], shingles[[bucket[j]]])

        if (score >= threshold) {
          from[k] = bucket[i]
          to[k] = bucket[j]
          k <- k + 1L
        }
      }
    }
  }

  from <- from[seq_len(k - 1)]
  to <- to[seq_len(k - 1)]

  list(from = from, to = to)
}
