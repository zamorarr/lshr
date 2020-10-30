test_that("group_edges works with sequential nodes", {
  from <- c(0, 0, 1, 1, 3, 4)
  to <- c(1, 2, 0, 2, 4, 5)

  actual <- group_edges(from, to)

  expect_identical(actual$node, c(0:5))
  expect_identical(actual$group, as.integer(c(1, 1, 1, 2, 2, 2)))
})

test_that("group_edges works with non-sequential nodes", {
  from <- c(1, 1, 2, 2, 4, 5, 100)
  to <- c(2, 3, 1, 3, 5, 6, 101)

  actual <- group_edges(from, to)

  expect_identical(actual$node, c(1:6, 100:101))
  expect_identical(actual$group, as.integer(c(1, 1, 1, 2, 2, 2, 3, 3)))
})

