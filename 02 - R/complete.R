complete <- function(directory,id=1:332) {
      x<-id
      id<-formatC(id,flag="0",width=3)
      id<-paste(id,".csv",sep="")
      a<-data.frame(id=integer(),nobs=integer())
      for (i in 1:length(id)) {
            b<-read.csv(file.path(directory,id[i]))
            a[i,"id"]<-x[i]
            mask<-!is.na(b[,"nitrate"]) & !is.na(b[,"sulfate"])
            a[i,"nobs"]<-sum(mask)
      }
      a
}