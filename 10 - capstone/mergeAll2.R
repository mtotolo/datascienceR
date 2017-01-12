mergeAll2<-function(x,n,indexes) {
      df1<-as.data.frame(read.csv(paste0("news/",x,"Part1.csv"),nrows = n),stringAsFactors=F)
      names(df1)<-c("label","freq")
      df1<-df1 %>% group_by(label) %>% summarise_each(funs(sum))
      df1 %>% ungroup()
      df1$label<-as.character(df1$label)
      df2<-as.data.frame(read.csv(paste0("twitter/",x,"Part1.csv"),nrows = n),stringAsFactors=F)
      names(df2)<-c("label","freq")
      df2<-df2 %>% group_by(label) %>% summarise_each(funs(sum))
      df2 %>% ungroup()
      df2$label<-as.character(df2$label)
      df3<-as.data.frame(read.csv(paste0("blogs/",x,"Part1.csv"),nrows = n),stringAsFactors=F)
      names(df3)<-c("label","freq")
      df3<-df3 %>% group_by(label) %>% summarise_each(funs(sum))
      df3 %>% ungroup()
      df3$label<-as.character(df3$label)
      type<-c("news/","twitter/","blogs/")
      dff<-c(df1,df2,df3)
      for (j in 1:3) {
            print(j)
            dfx<-dff[j]
            for (i in indexes) {
                  print(paste0("--part",i,"---"))
                  df2<-as.data.frame(read.csv(paste0(type[j],x,"Part",i,".csv"),
                                              nrows = n),
                                     stringAsFactors=F)
                  names(df2)<-c("label","freq2")
                  df2<-df2 %>% group_by(label) %>% summarise_each(funs(sum))
                  df2 %>% ungroup()
                  df2$label<-as.character(df2$label)
                  dfx<-full_join(dfx,df2,by="label")
                  dfx<-dfx %>% rowwise %>% mutate(freq=sum(freq,freq2,na.rm=T)) %>% 
                        select(-freq2)
            }
            
            dff[j] <- dfx %>% arrange(desc(freq))
      }
      return(dff)
}