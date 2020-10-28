test_that("shingle by words works", {
  x <- c("hello my name is bob", "what is your name")

  actual <- shingle(x, tokenizers::tokenize_words)
  expected <- list(
    as.integer(c(1, 2, 3, 4, 5)),
    as.integer(c(6, 4, 7, 3))
  )

  expect_identical(actual, expected)
})

test_that("shingle_words produces sets (shingle repeats)", {
  x <- c("hello my name is bob hello", "what what is your name")

  actual <- shingle(x, tokenizers::tokenize_words)
  expected <- list(
    as.integer(c(1, 2, 3, 4, 5)),
    as.integer(c(6, 4, 7, 3))
  )

  expect_identical(actual, expected)
})

test_that("jaccard_shingles works", {
  x <- c("hello hello my name is bob", "hi my name is alice")
  s <- shingle(x, tokenizers::tokenize_words)

  # intersection: my name is (3)
  # union: hello my name is bob hi alice (7)
  actual <- jaccard_shingles(s[[1]], s[[2]])
  expected <- 3/7

  expect_equal(actual, expected)
})
