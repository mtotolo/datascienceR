Len=44964
MaxLen=899288
bigramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
trigramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
quadgramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
}

for (i in 1:20) {
      sk<-(i-1)*Len
      if (i!=20) readN<-Len
      else readN <- -1
      selectedrows<-scan("Coursera-SwiftKey/final/en_US/en_US.blogs.txt",what="character",
            n=readN,sep="\n", skip=sk,quiet=T,encoding = "UTF-8")
      myCorpus<-Corpus(VectorSource(selectedrows))
      print(paste0(sk,"--",readN))
      myCorpus<-preProcess(myCorpus,profanity)
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=bigramTokenizer))
      freq <- sort(col_sums(dtm),decreasing = T)
      fileName<-paste0("Part",i,".csv")
      write.csv(freq,file=fileName)
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=trigramTokenizer))
      freq <- sort(col_sums(dtm),decreasing = T)
      fileName<-paste0("TriPart",i,".csv")
      write.csv(freq,file=fileName)
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=quadgramTokenizer))
      freq <- sort(col_sums(dtm),decreasing = T)
      fileName<-paste0("QuadPart",i,".csv")
      write.csv(freq,file=fileName)
      rm(myCorpus)
      rm(dtm)
      rm(freq)
}
