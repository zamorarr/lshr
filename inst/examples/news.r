news_sample <- dplyr::sample_n(news, 4000)
x <- news_sample$title

s <- shingle(x, function(x) tokenizers::tokenize_character_shingles(x, 5))
results <- lsh(s)

edges <- prune_results(results, s, 0.9)
groups <- group_edges_rcpp(edges$from, edges$to)
groups$text <- x[groups$node]
dplyr::arrange(tibble::as_tibble(groups), group)
