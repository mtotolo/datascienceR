pollutantmean <- function(directory,pollutant,id=1:332) {
      id<-formatC(id,flag="0",width=3)
      id<-paste(id,".csv",sep="")
      for (i in 1:length(id)) {
            if (i>1) {
                  b<-read.csv(file.path(directory,id[i])) 
                  a<-rbind(a,b)
            }
            else {
                  a<-read.csv(file.path(directory,id[i]))
            }
      }
      mean(a[,pollutant],na.rm=TRUE)
}