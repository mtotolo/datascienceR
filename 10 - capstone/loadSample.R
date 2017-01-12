loadSample <- function(what,p) {
      currdir<-getwd()
      setwd("C:/Users/Marco/Desktop/coursera/capstone/Coursera-SwiftKey/final")
      # number of lines of en_US.news=77254
      filelength=77254
      textfile<-file(paste0("en_US/en_US.",what,".txt"),open="r")
      rowselect=rbinom(filelength,1,p)
      print(paste0("Loading ",sum(rowselect), " documents"))
      selectedrows<-NULL
      y=0
      for (i in rowselect) {
            if(i) {
                  selectedrows<-c(selectedrows,
                                  scan(textfile,what="character",n=1,sep="\n",skip=y))
                  
            y=0
            }
            else y<-y+1
      }
      close(textfile)
      setwd(currdir)
      return(selectedrows)
}

loadSample2 <- function(what,p) {
      currdir<-getwd()
      setwd("C:/Users/Marco/Desktop/coursera/capstone/Coursera-SwiftKey/final")
      # number of lines of en_US.news=77254
      textfile<-file(paste0("en_US/en_US.",what,".txt"),open="r")
      print("Computing file length")
      filelength=length(readLines(textfile))
      close(textfile)
      textfile<-file(paste0("en_US/en_US.",what,".txt"),open="r")
      rowselect=rbinom(filelength,1,p)
      print(paste0("Loading ",sum(rowselect), " documents"))
      selectedrows<-NULL
      y=0
      for (i in rowselect) {
            if(i) {
                  selectedrows<-c(selectedrows,
                                  scan(textfile,what="character",n=1,sep="\n",
                                       skip=y,quiet=T,encoding = "UTF-8"))
                  
                  y=0
            }
            else y<-y+1
      }
      close(textfile)
      setwd(currdir)
      myCorpus<-Corpus(VectorSource(selectedrows))
      return(myCorpus)
}


loadSample3 <- function(what,p) {
      currdir<-getwd()
      setwd("C:/Users/Marco/Desktop/coursera/capstone/Coursera-SwiftKey/final")
      # number of lines of en_US.news=77254
      textfile<-paste0("en_US/en_US.",what,".txt")
      
      myCorpus<-Corpus(DirSource(textfile))
      setwd(currdir)
      return(myCorpus)
}

loadSample4 <- function(what,p) {
      currdir<-getwd()
      setwd("C:/Users/Marco/Desktop/coursera/capstone/Coursera-SwiftKey/final")
      # number of lines of en_US.news=77254
      textfile<-file(paste0("en_US/en_US.",what,".txt"),open="r")
      print("Computing file length")
      filelength=length(readLines(textfile))
      close(textfile)
      textfile<-file(paste0("en_US/en_US.",what,".txt"),open="r")
      rowselect=rbinom(filelength,1,p)
      print(paste0("Loading ",sum(rowselect), " documents"))
      selectedrows<-NULL
      y=0
      for (i in rowselect) {
            if(i) {
                  selectedrows<-c(selectedrows,
                                  scan(textfile,what="character",n=1,sep="\n",
                                       skip=y,quiet=T,encoding = "UTF-8"))
                  
                  y=0
            }
            else y<-y+1
      }
      close(textfile)
      setwd(currdir)
      myCorpus<-Corpus(VectorSource(selectedrows))
      return(myCorpus)
}
