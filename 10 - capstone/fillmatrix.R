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
BiAll<-filter(BiAll, pre %in% mydictionary$label & post %in% mydictionary$label)
TriAll<-filter(TriAll, pre1 %in% mydictionary$label & post %in% mydictionary$label &
                     pre2 %in% mydictionary$label)
QuadAll<-filter(QuadAll, pre1 %in% mydictionary$label & post %in% mydictionary$label &
                     pre2 %in% mydictionary$label & pre3 %in% mydictionary$label)

mydictionary<-filter(UniAll,freq>N-1)
bimatrix<-simple_sparse_zero_array(dim=rep(nrow(mydictionary),2))
trimatrix<-simple_sparse_zero_array(dim=rep(nrow(mydictionary),3))
quadmatrix<-simple_sparse_zero_array(dim=rep(nrow(mydictionary),4))
saveRDS(mydictionary,"mydictionary.rds")
print("bi")
for (i in 1:nrow(BiAll)) {
      #      if (i %% 10000==0) print(i)
      bimatrix[which(mydictionary$label==BiAll[i,]$pre),
                which(mydictionary$label==BiAll[i,]$post)
                ]<-BiAll[i,]$freq
}
saveRDS(bimatrix,"bimatrix.rds")
print("tri")
for (i in 1:nrow(TriAll)) {
#      if (i %% 10000==0) print(i)
      trimatrix[which(mydictionary$label==TriAll[i,]$pre1),
               which(mydictionary$label==TriAll[i,]$pre2),
               which(mydictionary$label==TriAll[i,]$post)
               ]<-TriAll[i,]$freq
}
saveRDS(trimatrix,"trimatrix.rds")
print("quad")
for (i in 1:nrow(QuadAll)) {
      #      if (i %% 10000==0) print(i)
      quadmatrix[which(mydictionary$label==QuadAll[i,]$pre1),
                which(mydictionary$label==QuadAll[i,]$pre2),
                which(mydictionary$label==QuadAll[i,]$pre3),
                which(mydictionary$label==QuadAll[i,]$post)
                ]<-QuadAll[i,]$freq
}
saveRDS(quadmatrix,"quadmatrix.rds")