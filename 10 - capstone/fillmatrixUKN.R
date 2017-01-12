require(tidyr)
require(dplyr)
require(slam)
N=70
UniAll<-read.csv("UniAll.csv")
BiAll<-read.csv("BiAll.csv")
TriAll<-read.csv("TriAll.csv")
QuadAll<-read.csv("QuadAll.csv")
BiAll<-separate(BiAll,label,c("pre","post"),sep=" ")
TriAll<-separate(TriAll,label,c("pre1","pre2","post"),sep=" ")
QuadAll<-separate(QuadAll,label,c("pre1","pre2","pre3","post"),sep=" ")
BiAll<-filter(BiAll,freq>1)
TriAll<-filter(TriAll,freq>1)
QuadAll<-filter(QuadAll,freq>1)
mydictionary<-filter(UniAll,freq>N-1)
print("processing UNK")

BiAllUNK<-filter(BiAll, !(pre %in% mydictionary$label))
BiAllUNK<-filter(BiAllUNK, post %in% mydictionary$label)
BiAllUNK <- aggregate(BiAllUNK$freq,by=list(post=BiAllUNK$post),FUN=sum)
BiAllUNK$pre <- "<UNK>"
names(BiAllUNK)<-c("post","freq","pre")
BiAllUNK <- arrange(BiAllUNK,desc(freq))
BiAll<-filter(BiAll, pre %in% mydictionary$label & post %in% mydictionary$label)
BiAll<-full_join(BiAll,BiAllUNK)
BiAll<-arrange(BiAll,desc(freq))

print("processing 3UNK")

TriAllUNK1<-filter(TriAll, !(pre1 %in% mydictionary$label))
TriAllUNK1<-filter(TriAllUNK1, post %in% mydictionary$label & pre2 %in% mydictionary$label)
TriAllUNK1 <- aggregate(TriAllUNK1$freq,by=list(post=TriAllUNK1$post,pre2=TriAllUNK1$pre2),
                        FUN=sum)
TriAllUNK1$pre1 <- "<UNK>"
names(TriAllUNK1)<-c("post","pre2","freq","pre1")
TriAllUNK1 <- arrange(TriAllUNK1,desc(freq))

TriAllUNK2<-filter(TriAll, !(pre2 %in% mydictionary$label))
TriAllUNK2<-filter(TriAllUNK2, post %in% mydictionary$label & pre1 %in% mydictionary$label)
TriAllUNK2 <- aggregate(TriAllUNK2$freq,by=list(post=TriAllUNK2$post,pre1=TriAllUNK2$pre1),
                        FUN=sum)
TriAllUNK2$pre2 <- "<UNK>"
names(TriAllUNK2)<-c("post","pre1","freq","pre2")
TriAllUNK2 <- arrange(TriAllUNK2,desc(freq))

TriAll<-filter(TriAll, pre1 %in% mydictionary$label & post %in% mydictionary$label &
                     pre2 %in% mydictionary$label)
TriAll<-full_join(TriAll,TriAllUNK1)
TriAll<-full_join(TriAll,TriAllUNK2)
TriAll<-arrange(TriAll,desc(freq))

print("processing 4UNK")

QuadAllUNK1<-filter(QuadAll, !(pre1 %in% mydictionary$label))
QuadAllUNK1<-filter(QuadAllUNK1, post %in% mydictionary$label & pre2 %in% mydictionary$label &
                          pre3 %in% mydictionary$label)
QuadAllUNK1 <- aggregate(QuadAllUNK1$freq,by=list(post=QuadAllUNK1$post,pre2=QuadAllUNK1$pre2,
                                                  pre3=QuadAllUNK1$pre3),
                        FUN=sum)
QuadAllUNK1$pre1 <- "<UNK>"
names(QuadAllUNK1)<-c("post","pre2","pre3","freq","pre1")
QuadAllUNK1 <- arrange(QuadAllUNK1,desc(freq))

QuadAllUNK2<-filter(QuadAll, !(pre2 %in% mydictionary$label))
QuadAllUNK2<-filter(QuadAllUNK2, post %in% mydictionary$label & pre1 %in% mydictionary$label &
                          pre3 %in% mydictionary$label)
QuadAllUNK2 <- aggregate(QuadAllUNK2$freq,by=list(post=QuadAllUNK2$post,pre1=QuadAllUNK2$pre1,
                                                  pre3=QuadAllUNK2$pre3),
                         FUN=sum)
QuadAllUNK2$pre2 <- "<UNK>"
names(QuadAllUNK2)<-c("post","pre1","pre3","freq","pre2")
QuadAllUNK2 <- arrange(QuadAllUNK2,desc(freq))

QuadAllUNK3<-filter(QuadAll, !(pre3 %in% mydictionary$label))
QuadAllUNK3<-filter(QuadAllUNK3, post %in% mydictionary$label & pre1 %in% mydictionary$label &
                          pre2 %in% mydictionary$label)
QuadAllUNK3 <- aggregate(QuadAllUNK3$freq,by=list(post=QuadAllUNK3$post,pre1=QuadAllUNK3$pre1,
                                                  pre2=QuadAllUNK3$pre2),
                         FUN=sum)
QuadAllUNK3$pre3 <- "<UNK>"
names(QuadAllUNK3)<-c("post","pre1","pre2","freq","pre3")
QuadAllUNK3 <- arrange(QuadAllUNK3,desc(freq))

QuadAllUNK4<-filter(QuadAll, !(pre1 %in% mydictionary$label) & !(pre2 %in% mydictionary$label))
QuadAllUNK4<-filter(QuadAllUNK4, post %in% mydictionary$label & pre3 %in% mydictionary$label)
QuadAllUNK4 <- aggregate(QuadAllUNK4$freq,by=list(post=QuadAllUNK4$post,pre3=QuadAllUNK4$pre3),
                         FUN=sum)
QuadAllUNK4$pre1 <- "<UNK>"
QuadAllUNK4$pre2 <- "<UNK>"
names(QuadAllUNK4)<-c("post","pre3","freq","pre1","pre2")
QuadAllUNK4 <- arrange(QuadAllUNK4,desc(freq))

QuadAllUNK5<-filter(QuadAll, !(pre2 %in% mydictionary$label) & !(pre3 %in% mydictionary$label))
QuadAllUNK5<-filter(QuadAllUNK5, post %in% mydictionary$label & pre1 %in% mydictionary$label)
QuadAllUNK5 <- aggregate(QuadAllUNK5$freq,by=list(post=QuadAllUNK5$post,pre1=QuadAllUNK5$pre1),
                         FUN=sum)
QuadAllUNK5$pre2 <- "<UNK>"
QuadAllUNK5$pre3 <- "<UNK>"
names(QuadAllUNK5)<-c("post","pre1","freq","pre2","pre3")
QuadAllUNK5 <- arrange(QuadAllUNK5,desc(freq))

QuadAllUNK6<-filter(QuadAll, !(pre1 %in% mydictionary$label) & !(pre3 %in% mydictionary$label))
QuadAllUNK6<-filter(QuadAllUNK6, post %in% mydictionary$label & pre2 %in% mydictionary$label)
QuadAllUNK6 <- aggregate(QuadAllUNK6$freq,by=list(post=QuadAllUNK6$post,pre2=QuadAllUNK6$pre2),
                         FUN=sum)
QuadAllUNK6$pre1 <- "<UNK>"
QuadAllUNK6$pre3 <- "<UNK>"
names(QuadAllUNK6)<-c("post","pre2","freq","pre1","pre3")
QuadAllUNK6 <- arrange(QuadAllUNK6,desc(freq))

QuadAll<-filter(QuadAll, pre1 %in% mydictionary$label & post %in% mydictionary$label &
                      pre2 %in% mydictionary$label & pre3 %in% mydictionary$label)

QuadAll<-full_join(QuadAll,QuadAllUNK1)
QuadAll<-full_join(QuadAll,QuadAllUNK2)
QuadAll<-full_join(QuadAll,QuadAllUNK3)
QuadAll<-full_join(QuadAll,QuadAllUNK4)
QuadAll<-full_join(QuadAll,QuadAllUNK5)
QuadAll<-full_join(QuadAll,QuadAllUNK6)
QuadAll<-arrange(QuadAll,desc(freq))

mydictionary<-rbind(mydictionary,data.frame(label="<UNK>",freq=3500000))

bimatrix<-simple_sparse_zero_array(dim=rep(nrow(mydictionary),2))
trimatrix<-simple_sparse_zero_array(dim=rep(nrow(mydictionary),3))
quadmatrix<-simple_sparse_zero_array(dim=rep(nrow(mydictionary),4))
saveRDS(mydictionary,"mydictionaryUNK.rds")
print("bi")
for (i in 1:nrow(BiAll)) {
      #      if (i %% 10000==0) print(i)
      bimatrix[which(mydictionary$label==BiAll[i,]$pre),
               which(mydictionary$label==BiAll[i,]$post)
               ]<-BiAll[i,]$freq
}
saveRDS(bimatrix,"bimatrixUNK.rds")
print("tri")
for (i in 1:nrow(TriAll)) {
      #      if (i %% 10000==0) print(i)
      trimatrix[which(mydictionary$label==TriAll[i,]$pre1),
                which(mydictionary$label==TriAll[i,]$pre2),
                which(mydictionary$label==TriAll[i,]$post)
                ]<-TriAll[i,]$freq
}
saveRDS(trimatrix,"trimatrixUNK.rds")
print("quad")
for (i in 1:nrow(QuadAll)) {
      #      if (i %% 10000==0) print(i)
      quadmatrix[which(mydictionary$label==QuadAll[i,]$pre1),
                 which(mydictionary$label==QuadAll[i,]$pre2),
                 which(mydictionary$label==QuadAll[i,]$pre3),
                 which(mydictionary$label==QuadAll[i,]$post)
                 ]<-QuadAll[i,]$freq
}
saveRDS(quadmatrix,"quadmatrixUNK.rds")