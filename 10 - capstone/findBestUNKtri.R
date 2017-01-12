require(dplyr)
require(slam)
require(triebeard)
mydictionary<-readRDS("mydictionaryUNK.rds")
mydictionary$label<-as.character(mydictionary$label)
bimatrix<-readRDS("bimatrixUNK.rds")
trimatrix<-readRDS("trimatrixUNK.rds")
quadmatrix<-readRDS("quadmatrixUNK.rds")
Uindex<-nrow(mydictionary)
mytree<-trie(keys=mydictionary$label,values=1:nrow(mydictionary))
filler<-function(x) {
      NAs<-sum(is.na(x))
      if (NAs==0) return(x)
      if (NAs==5) return(mydictionary[1:5,1])
      j<-1
      for (i in length(x)-NAs+1:length(x)-1) {
            while (is.na(x[i])) {
                  y<-mydictionary[j,1]
                  if(!(y %in% x)) x[i]<-y
                  j<-j+1
            }
      }
      return(unique(x)[1:5])
}
fillerfun<-function(x,what) {
      NAs<-sum(is.na(x))
      if (NAs==0) return(x)
      if (NAs==5) return(mydictionary[1:5,1])
      x<-x[!is.na(x)]
      x<-c(x,what)
      return(unique(x)[1:5])
}
findBest2U <-function(x) {
      #      return(mydictionary[which.max(as.vector(bimatrix[which(mydictionary$label==x),])),]) 
      x<-x[length(x)]
      if (!(x %in% mydictionary$label)) x<-"<UNK>"
      i<-longest_match(mytree,x)
      indexes<-order(bimatrix[i,]$v,decreasing=T)[1:5]
      return(filler(mydictionary[drop_simple_sparse_array(bimatrix[i,])$i[indexes],1]))
}

findBest3U <-function(x) {
      
      #      return(mydictionary[which.max(as.vector(trimatrix[which(mydictionary$label==x),
      #                                                      which(mydictionary$label==y),])),]) 
      x<-x[(length(x)-1):length(x)]
#      i<-which(mydictionary$label==x[1])
#      j<-which(mydictionary$label==x[2])
      if (!(x[1] %in% mydictionary$label)) x[1]<-"<UNK>"
      if (!(x[2] %in% mydictionary$label)) x[2]<-"<UNK>"
      a<-longest_match(mytree,x)
      i<-a[1]
      j<-a[2]
      indexes<-order(trimatrix[i,j,]$v,decreasing=T)[1:5]            
      if (!all(is.na(indexes)))
            return(fillerfun(mydictionary[
                  drop_simple_sparse_array(trimatrix[i,j,])$i[indexes],1],
                  findBest2U(x[2])))
      return(findBest2U(x[2]))
}

findBest4U <-function(x) {
      
      #     return(mydictionary[which.max(as.vector(quadmatrix[which(mydictionary$label==x),
      #                                                      which(mydictionary$label==y),
      #                                              which(mydictionary$label==z),])),]) 
      x<-x[(length(x)-2):length(x)]
      if (!(x[1] %in% mydictionary$label)) x[1]<-"<UNK>"
      if (!(x[2] %in% mydictionary$label)) x[2]<-"<UNK>"
      if (!(x[3] %in% mydictionary$label)) x[3]<-"<UNK>"
      a<-longest_match(mytree,x)
      i<-a[1]
      j<-a[2]
      k<-a[3]
      indexes<-order(quadmatrix[i,j,k,]$v,decreasing=T)[1:5]
            if (!all(is.na(indexes)))
                  return(fillerfun(mydictionary[
                        drop_simple_sparse_array(quadmatrix[i,j,k,])$i[indexes],1],
                        findBest3U(x[2:3])))
      return(findBest3U(x[2:3]))
}