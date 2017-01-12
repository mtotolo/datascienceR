corr <- function(directory,threshold=0) {
      files<-list.files(path=directory,full.names=T)
      selection<-complete(directory)[,"nobs"]>threshold
      a<-vector("numeric")
      for (i in (files[selection])) {
            data<-read.csv(i)
            b<-cor(data[,"nitrate"],data[,"sulfate"],use="complete.obs")
            a<-c(a,b)
      }
      a
}