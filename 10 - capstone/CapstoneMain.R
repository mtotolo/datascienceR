library(slam)
library(tm)
library(SnowballC)
library(wordcloud)
setwd("C:\\Users\\Marco\\Desktop\\coursera\\capstone")
source("loadSample.R")
source("preProcess.R")
source("explore.R")

set.seed(123)

### Loading and Sampling Data
print("**Loading Data**")
SampledDocs<-loadSample3("blogs",1) #Here choose which file and sampling

### Tokenize text
## done with mt package

#tt<-as.Token_Tokenizer(wordpunct_tokenizer) #Here choose wordpunct_ or whitespace_
#NewsTokenized<-sapply(FUN=tt,NewsSampled,USE.NAMES = F) #Create list with tokens
#NewsTokenized<-sapply(FUN=as.character,NewsTokenized) #Typecast for single words needed

#Tokenized<-sapply(FUN=MC_tokenizer,Sampled,USE.NAMES = F) #Create list with tokens
#Tokenized<-sapply(FUN=MC_tokenizer,Tokenized,USE.NAMES = F) #deletes whitespaces
#Tokenized<-sapply(FUN=tolower,Tokenized,USE.NAMES = F) #all lowercase

print("**Preprecessing**")
profanity<-readLines("profanity2.txt")
SampledDocs<-preProcess(SampledDocs,profanity)

print("**Exploring Data**")
uni<-explore(SampledDocs)
bi<-exploreBi(SampledDocs)
tri<-exploreTri(SampledDocs)
quad<-exploreQuad(SampledDocs)
#delete_whitespace<-function(x)
#      x[x!=""]
#Tokenized<-sapply(FUN=delete_whitespace,Tokenized,USE.NAMES = F)

### Filtering Profanity
## basic profanity list downloaded from 
## https://gist.githubusercontent.com/jamiew/1112488/raw/7ca9b1669e1c24b27c66174762cb04e14cf05aa7/google_twunter_lol
