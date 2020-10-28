test_that("create_buckets works", {
  x <- c("alice", "bob", "charlie", "bob")

  actual <- create_buckets(x)
  expected <- list(
    alice = 1L,
    bob = c(2L, 4L),
    charlie = 3L
  )

  expect_identical(actual, expected)
})
