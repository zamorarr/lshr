from <- dplyr::dense_rank(sample.int(1E5, replace = TRUE))
to <- dplyr::dense_rank(sample.int(1E5, replace = TRUE))

# regular R:
# 10,000 length vector ~ 0.5 sec
# 30,000 length vector ~ 4 sec
# 100,000 length vector ~ 75 secs
profvis::profvis(r <- group_edges(from, to))

# Rcpp:
# 10,000 length vector ~ 0.02 sec
# 30,000 length vector ~ 0.25 sec
# 100,000 length vector ~ 2 sec
# 300,000 length vector ~ 15 sec
profvis::profvis(r <- group_edges_rcpp(from - 1L, to - 1L))

sort(table(group_edges(from, to)), decreasing = TRUE)

start_profiler("src/profile.out")
r <- group_edges_rcpp(from - 1L, to - 1L)
stop_profiler()

# google-pprof --text src/lshr.so src/profile.out
