#' Minhash Document Shingles
#'
#' @param shingles list of shingled documents (from \code{shingle_*()} functions)
#' @param n number of minhashes per document
#' @param seed optional seed used when generating minhash functions
#'
#' @export
minhash <- function(shingles, n = 100, seed = NULL) {
  # generate minhasher
  f <- minhash_generator(n, seed)

  # generate minhash for every shingle
  vals <- lapply(shingles, f)

  # build minhash matrix
  matrix(unlist(vals), nrow = n)
}

#' Generate n random integers
#'
#' Used to parametrize the minhasher
#' @param n number of integers
#' @param seed optional seed
random_ints <- function(n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  sample.int(.Machine$integer.max, size = n)
}

#' Generate a minhash function
#'
#' @param n number of minhashes per document
#' @param seed optional seed used when generating minhash functions
minhash_generator <- function(n, seed = NULL) {
  rs <- random_ints(n, seed)

  # return generator function
  # x is a shingled document
  function(x) {
    # get minhash for every hash function (paramaterized by a random int from rs)
    vapply(rs, function(r) min(bitwXor(x, r)), integer(1L), USE.NAMES = FALSE)
  }
}

#' @export
jaccard_minhashes <- function(x, y) {
  mean(x == y)
}
