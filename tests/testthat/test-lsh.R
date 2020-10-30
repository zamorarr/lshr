test_that("create_buckets works", {
  x <- c("alice", "bob", "charlie", "bob")

  actual <- createBuckets(x)
  expected <- list(
    alice = 1L,
    bob = c(2L, 4L),
    charlie = 3L
  )

  expect_length(actual, 3)
  expect_identical(actual$alice, expected$alice)
  expect_identical(actual$bob, expected$bob)
  expect_identical(actual$charlie, expected$charlie)
})
