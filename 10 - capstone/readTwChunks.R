print("**Computing length**")
MaxLen=length(readLines("Coursera-SwiftKey/final/en_US/en_US.news.txt",
                 encoding="UTF-8"))
print(MaxLen)
Len=floor(MaxLen/20)

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
      print("**Reading chunk**")
      selectedrows<-scan("Coursera-SwiftKey/final/en_US/en_US.news.txt",what="character",
                  n=readN,sep="\n", skip=sk,quiet=T,encoding = "UTF-8")
      selectedrows <- sapply(selectedrows,function(row) iconv(row, "latin1", "ASCII", sub=""))
      myCorpus<-Corpus(VectorSource(selectedrows))
      print(paste0(i,"---",sk,"--",readN))
      myCorpus<-preProcess(myCorpus,profanity)
      print("uni")
      dtm <- DocumentTermMatrix(myCorpus)
      freq <- sort(col_sums(dtm),decreasing = T)
      fileName<-paste0("news/Part",i,".csv")
      write.csv(freq,file=fileName)
      print("bi")
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=bigramTokenizer))
      freq <- sort(col_sums(dtm),decreasing = T)
      fileName<-paste0("news/BiPart",i,".csv")
      write.csv(freq,file=fileName)
      print("tri")
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=trigramTokenizer))
      freq <- sort(col_sums(dtm),decreasing = T)
      fileName<-paste0("news/TriPart",i,".csv")
      write.csv(freq,file=fileName)
      print("quad")
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=quadgramTokenizer))
      freq <- sort(col_sums(dtm),decreasing = T)
      fileName<-paste0("news/QuadPart",i,".csv")
      write.csv(freq,file=fileName)
      rm(myCorpus)
      rm(dtm)
      rm(freq)
}
