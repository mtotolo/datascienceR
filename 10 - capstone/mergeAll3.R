mergeAll3<-function(x,n,indexes) {
      df1<-as.data.frame(read.csv(paste0(x,"1.csv"),nrows = n),stringAsFactors=F)
      names(df1)<-c("label","freq")
      df1<-df1 %>% group_by(label) %>% summarise_each(funs(sum))
      df1 %>% ungroup()
      df1$label<-as.character(df1$label)
      for (i in indexes) {
            print(paste0("--part",i,"---"))
            df2<-as.data.frame(read.csv(paste0(x,i,".csv"),nrows = n),stringAsFactors=F)
            names(df2)<-c("label","freq2")
            df2<-df2 %>% group_by(label) %>% summarise_each(funs(sum))
            df2 %>% ungroup()
            df2$label<-as.character(df2$label)
            df1<-full_join(df1,df2,by="label")
            df1<-df1 %>% rowwise %>% mutate(freq=sum(freq,freq2,na.rm=T)) %>% 
                  select(-freq2)
      }
      df1 <- df1 %>% arrange(desc(freq))
      return(df1)
      
      
}
merge<- function(type,n,indexes) {
      files<-c(paste0("news\\",type,"Part"),
                  paste0("blogs\\",type,"Part"),
                  paste0("twitter\\",type,"Part"))
      News<-mergeAll3(files[1],n,indexes)
      Blogs<-mergeAll3(files[2],n,indexes)
      Twitter<-mergeAll3(files[3],n,indexes)
      df<-full_join(News,Blogs,by="label")
      df<-df %>% rowwise %>% mutate(freq=sum(freq.x,freq.y,na.rm=T)) %>% 
            select(-c(freq.x,freq.y))
      df<-full_join(df,Twitter,by="label")
      df<-df %>% rowwise %>% mutate(freq=sum(freq.x,freq.y,na.rm=T)) %>% 
            select(-c(freq.x,freq.y))
      df <- df %>% arrange(desc(freq))
      return(df)
}
