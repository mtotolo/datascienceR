bigramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
trigramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
quadgramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
}
explore <- function(myCorpus) {
      print("Computing dtm/tdm")
      dtm <- DocumentTermMatrix(myCorpus)
      #tdm <- TermDocumentMatrix(myCorpus) 
      print("Computing term frequencies")
      freq <- sort(col_sums(dtm),decreasing = T)  
      print("Most frequent terms:")
      print(head(freq,20))
#      wordcloud(names(freq), freq, max.words=50)

   
      return(freq)
}
exploreBi <- function(myCorpus) {
      print("Computing dtm/tdm")
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=bigramTokenizer))
      #tdm <- TermDocumentMatrix(myCorpus) 
      print("Computing bigram frequencies")
      freq <- sort(col_sums(dtm),decreasing = T)  
      print("Most frequent bigrams:")
      print(head(freq,20))
      #      wordcloud(names(freq), freq, max.words=50)

  
      return(freq)
}
exploreTri <- function(myCorpus) {
      print("Computing dtm/tdm")
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=trigramTokenizer))
      #tdm <- TermDocumentMatrix(myCorpus) 
      print("Computing trigram frequencies")
      freq <- sort(col_sums(dtm),decreasing = T)  
      print("Most frequent trigrams:")
      print(head(freq,20))
      #      wordcloud(names(freq), freq, max.words=50)
  
      return(freq)
}
exploreQuad <- function(myCorpus) {
      print("Computing dtm/tdm")
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=quadgramTokenizer))
      #tdm <- TermDocumentMatrix(myCorpus) 
      print("Computing quadgram frequencies")
      freq <- sort(col_sums(dtm),decreasing = T)  
      print("Most frequent quadgrams:")
      print(head(freq,20))
      #      wordcloud(names(freq), freq, max.words=50)
    
      return(freq)
}