test_that("multiplication works", {
  x <- c("hey there bob", "hey there bob", "hey there alice", "hey there alice", "hey there charlie")
  s <- shingle(x, tokenizers::tokenize_words)
  candidates <- lsh(s, 100, 5)

  actual <- build_edges(candidates, s, 0.8)

  expect_identical(ncol(actual), 2L)
  expect_identical(nrow(actual), 2L)
  expect_identical(actual$from, c(1L, 3L))
  expect_identical(actual$to, c(2L, 4L))
})

test_that("group_edges works with sequential nodes", {
  from <- c(0, 0, 1, 1, 3, 4)
  to <- c(1, 2, 0, 2, 4, 5)
  edges <- data.frame(from = from, to = to)

  actual <- group_edges(edges)

  expect_identical(actual$doc, c(0:5))
  expect_identical(actual$group, as.integer(c(1, 1, 1, 2, 2, 2)))
})

test_that("group_edges works with non-sequential nodes", {
  from <- c(1, 1, 2, 2, 4, 5, 100)
  to <- c(2, 3, 1, 3, 5, 6, 101)
  edges <- data.frame(from = from, to = to)

  actual <- group_edges(edges)

  expect_identical(actual$doc, c(1:6, 100:101))
  expect_identical(actual$group, as.integer(c(1, 1, 1, 2, 2, 2, 3, 3)))
})
