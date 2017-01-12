require(SnowballC)
require(slam)
require(tm)
require(dplyr)
require(tidyr)
preProcess2 <- function(myCorpus,toFilter) {
      print("aa")
      myCorpus <- tm_map(myCorpus, removePunctuation) # remove punctuation
      print("bb")
      myCorpus <- tm_map(myCorpus, tolower) # all lower case
      print("cc")
      myCorpus <- tm_map(myCorpus, removeWords, toFilter) # filter undesired words
      print("dd")
      myCorpus <- tm_map(myCorpus, removeNumbers) # remove numbers
      print("ee")
      myCorpus <- tm_map(myCorpus, removeWords, stopwords("english")) #remove stopwords
      print("ff")
      myCorpus <- tm_map(myCorpus, stripWhitespace) #remove whitespace
      print("gg")
      myCorpus <- tm_map(myCorpus, PlainTextDocument) #convert to plain text
      return(myCorpus)
}

profanity<-readLines("profanity.txt")
mysample1<-loadSample4("News",0.3)
mysample2<-loadSample4("Twitter",0.3)
mysample2<-loadSample4("Blogs",0.3)
print("preprocessing")
ProcessedDocs1<-preProcess2(mysample1,profanity)
ProcessedDocs2<-preProcess2(mysample2,profanity)
ProcessedDocs3<-preProcess2(mysample3,profanity)
print("processing dtm")
dtm1 <- DocumentTermMatrix(ProcessedDocs1)
dtm2 <- DocumentTermMatrix(ProcessedDocs2)
dtm3 <- DocumentTermMatrix(ProcessedDocs3)
print("processing frequencies")
freq1 <- as.data.frame(sort(col_sums(dtm1),decreasing = T))
freq2 <- as.data.frame(sort(col_sums(dtm2),decreasing = T))
freq3 <- as.data.frame(sort(col_sums(dtm2),decreasing = T))
names(freq1)<-c("label","freq")
names(freq2)<-c("label","freq")
names(freq3)<-c("label","freq")
freqAll<-full_join(freq1,freq2,by="label")
freqAll<-freqAll %>% rowwise %>% mutate(freq=sum(freq.x,freq.y,na.rm=T)) %>% 
      select(-c(freq.x,freq.y))
freqAll<-full_join(freqAll,freq3,by="label")
freqAll<-freqAll %>% rowwise %>% mutate(freq=sum(freq.x,freq.y,na.rm=T)) %>% 
      select(-c(freq.x,freq.y))
write.csv(freqAll,"UniNoStop.csv",row.names = F)
