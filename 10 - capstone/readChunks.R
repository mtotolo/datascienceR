library(tm)
library(slam)



bigramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
trigramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
# repeat for 20 chunks
for (j in c("news","twitter","blogs")) {
      MaxLen=length(readLines(paste0("Coursera-SwiftKey/final/en_US/en_US.",j,".txt"),
                              encoding="UTF-8"))
      Len=floor(MaxLen/20)
      
      for (i in 1:20) {
      print(i)
      sk<-(i-1)*Len
      if (i!=20) readN<-Len
      else readN <- -1
      selectedrows<-scan(paste0("Coursera-SwiftKey/final/en_US/en_US.",j,".txt"),
                         what="character", n=readN,sep="\n", skip=sk,quiet=T,
                         encoding = "UTF-8")
      # remove unrecognizable character with the function iconv
      selectedrows <- sapply(selectedrows,function(row) 
            iconv(row, "latin1", "ASCII", sub=""))
      # create Corpus object
      myCorpus<-Corpus(VectorSource(selectedrows))
      # load a list of undesired words and apply preprocessing on the Corpus.
      # see the preProcess function below for details.
      profanity<-readLines("profanity.txt")
      myCorpus<-preProcess2(myCorpus,profanity)
      # create a DocumentTermMatrix object
      dtm <- DocumentTermMatrix(myCorpus)
      # create word frequency vector
      freq <- sort(col_sums(dtm),decreasing = T)
      # save file to disk for later processing
      fileName<-paste0(j,"\\NoStopUniPart",i,".csv")
      write.csv(freq,file=fileName)
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=bigramTokenizer))
      # create word frequency vector
      freq <- sort(col_sums(dtm),decreasing = T)
      # save file to disk for later processing
      fileName<-paste0(j,"\\NoStopBiPart",i,".csv")
      write.csv(freq,file=fileName)
      dtm <- DocumentTermMatrix(myCorpus,control=list(tokenizer=trigramTokenizer))
      # create word frequency vector
      freq <- sort(col_sums(dtm),decreasing = T)
      # save file to disk for later processing
      fileName<-paste0(j,"\\NoStopTriPart",i,".csv")
      write.csv(freq,file=fileName)
      }
}
preProcess <- function(myCorpus,toFilter) {
      myCorpus <- tm_map(myCorpus, removePunctuation) # remove punctuation
      myCorpus <- tm_map(myCorpus, tolower) # all lower case
      myCorpus <- tm_map(myCorpus, removeWords, toFilter) # filter undesired words
      myCorpus <- tm_map(myCorpus, removeNumbers) # remove numbers
      myCorpus <- tm_map(myCorpus, stripWhitespace) #remove whitespace
      myCorpus <- tm_map(myCorpus, PlainTextDocument) #convert to plain text
      return(myCorpus)
}