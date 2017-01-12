BigramTokenizer <- function(x) {
            unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
TrigramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
