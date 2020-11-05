lsh_minhash <- function(m, rows_per_band = 5) {
  num_hashes <- nrow(m)
  bands <- seq(1, num_hashes, rows_per_band)

  results <- pbmcapply::pbmclapply(bands, function(b) {
    m_band <- m[b:(b + rows_per_band - 1L),]
    hashes <- apply(m_band, 2, digest::digest, "murmur32")
    buckets <- createBuckets(hashes)

    # Filter results with only one entry
    purrr::keep(buckets, ~ length(.x) > 1)
  })

  # clean up results
  results <- purrr::compact(results)
  results <- purrr::flatten(results)
  results[!duplicated(results)]
}


#' Locality Sensitivity Hashing
#'
#' @param s shingled document
#' @param hashes number of hash functions
#' @param rows number of rows per bands
#' @export
lsh <- function(s, hashes = 100, rows = 5) {
  m <- minhash(s, hashes)
  lsh_minhash(m, rows)
}
