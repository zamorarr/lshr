test_that("random_ints works", {
  n <- 10L
  actual <- random_ints(n)
  expect_length(actual, n)
  expect_type(actual, "integer")
})

test_that("minhash works when shingles are indentical", {
  x <- list(1:3, 1:3)
  num_hashes <- 10L
  actual <- minhash(x, num_hashes)

  expect_identical(actual[,1], actual[,2])
  expect_identical(nrow(actual), num_hashes)
  expect_identical(ncol(actual), length(x))
})

test_that("minhash works when shingles are not indentical", {
  x <- list(1:3, 1:4)
  num_hashes <- 10L
  actual <- minhash(x, num_hashes)

  expect_identical(nrow(actual), num_hashes)
  expect_identical(ncol(actual), length(x))
})
