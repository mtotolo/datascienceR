
myVec<-c("blogs/QuadPart","news/QuadPart","twitter/QuadPart")
for (i in myVec) {
      a<-mergeAll(i,50000)
      write.csv(a,file=paste0(i,".csv"),row.names = F)
      
}