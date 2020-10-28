test_that("group_edges works with sequential nodes", {
  from <- c(0, 0, 1, 1, 3, 4)
  to <- c(1, 2, 0, 2, 4, 5)

  actual <- group_edges_rcpp(from, to)

  expect_identical(actual$node, c(0:5))
  expect_identical(actual$group, as.integer(c(1, 1, 1, 2, 2, 2)))
})

test_that("group_edges works with non-sequential nodes", {
  from <- c(1, 1, 2, 2, 4, 5, 100)
  to <- c(2, 3, 1, 3, 5, 6, 101)

  actual <- group_edges_rcpp(from, to)

  expect_identical(actual$node, c(1:6, 100:101))
  expect_identical(actual$group, as.integer(c(1, 1, 1, 2, 2, 2, 3, 3)))
})

test_that("merge_groups works", {
  groups <- lapply(1:10, function(i) i)
  node_group <- 1:10
  from <- 1
  to <- 2

  actual <- merge_groups(groups, node_group, from, to)
  expected <- list(
    groups = groups,
    node_group = node_group
  )

  expected$groups[[1]] <- as.integer(c(1, 2))
  expected$groups[[2]] <- NA_integer_
  expected$node_group[1] <- 1L
  expected$node_group[2] <- 1L

  expect_identical(actual$groups, expected$groups)
  expect_identical(actual$node_group, expected$node_group)
})

