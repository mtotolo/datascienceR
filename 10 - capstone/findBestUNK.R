require(dplyr)
require(slam)
mydictionary<-readRDS("mydictionaryUNK.rds")
mydictionary$label<-as.character(mydictionary$label)
bimatrix<-readRDS("bimatrixUNK.rds")
trimatrix<-readRDS("trimatrixUNK.rds")
quadmatrix<-readRDS("quadmatrixUNK.rds")
Uindex<-nrow(mydictionary)
filler<-function(x) {
      NAs<-sum(is.na(x))
      if (NAs==0) return(x)
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
      x<-x[!is.na(x)]
      x<-c(x,what)
      return(unique(x)[1:5])
}
findBest2U <-function(x) {
      #      return(mydictionary[which.max(as.vector(bimatrix[which(mydictionary$label==x),])),]) 
      x<-x[length(x)]
      i<-which(mydictionary$label==x)
      if(length(i)==0) i<-Uindex
      indexes<-order(bimatrix[i,]$v,decreasing=T)[1:5]
      if (!all(is.na(indexes)))
            return(filler(mydictionary[
                  drop_simple_sparse_array(bimatrix[i,])$i[indexes],1]))
      return(mydictionary[1:5,1])           
}

findBest3U <-function(x) {
      
      #      return(mydictionary[which.max(as.vector(trimatrix[which(mydictionary$label==x),
      #                                                      which(mydictionary$label==y),])),]) 
      x<-x[(length(x)-1):length(x)]
      i<-which(mydictionary$label==x[1])
      j<-which(mydictionary$label==x[2])
      if (length(i) == 0) i<-Uindex
      if (length(j) == 0) j<-Uindex
      indexes<-order(trimatrix[i,j,]$v,decreasing=T)[1:5]            
      if (!all(is.na(indexes)))
            return(fillerfun(mydictionary[
                  drop_simple_sparse_array(trimatrix[i,j,])$i[indexes],1],
                  findBest2U(x[2])))
      return(findBest2U(x[2]))
}
findBest3ood <-function(x) {
      #      return(mydictionary[which.max(as.vector(trimatrix[which(mydictionary$label==x),
      #                                                      which(mydictionary$label==y),])),]) 
      x<-x[(length(x)-1):length(x)]
      i<-which(mydictionary$label==x[1])
      j<-which(mydictionary$label==x[2])
      if (length(i)>0 & length(j)>0) {      
            indexes<-order(trimatrix[i,j,]$v,decreasing=T)[1:5]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(fillerfun(mydictionary[
                        drop_simple_sparse_array(trimatrix[i,j,])$i[indexes],1],
                        findBest2(x[2])))
            else return(findBest2(x[2]))
      }
      #            print("in")
      #            vec<-as.vector(trimatrix[which(mydictionary$label==x[1]),
      #                               which(mydictionary$label==x[2]),])
      #            if (max(vec)>0) return(mydictionary[order(vec,decreasing=T)[1:5],])
      #            else return(findBest2(x[2]))
      if (length(i)>0) {
            #            print("1 in")
            indexes<-order(trimatrix[i,,]$v,decreasing=T)[1:20]
            if (length(indexes>0) & !all(is.na(indexes)))
                  return(fillerfun(mydictionary[unique(
                        drop_simple_sparse_array(trimatrix[i,,])$i[indexes,2])[1:5],1],
                        findBest2(x[2])))
            else return(findBest2(x[2]))
      }
      if (length(j)>0) {
            #            print("2 in")
            indexes<-order(trimatrix[,j,]$v,decreasing=T)[1:20]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(filler(mydictionary[unique(
                        drop_simple_sparse_array(trimatrix[,j,])$i[indexes,2])[1:5],1],
                        findBest2(x[2])))
            
      }
      return(mydictionary[1:5,1])
}

findBest3UnoRoll <-function(x) {
      
      #      return(mydictionary[which.max(as.vector(trimatrix[which(mydictionary$label==x),
      #                                                      which(mydictionary$label==y),])),]) 
      i<-which(mydictionary$label==x[1])
      j<-which(mydictionary$label==x[2])
      if (length(i) == 0) i<-Uindex
      if (length(j) == 0) j<-Uindex
      indexes<-order(trimatrix[i,j,]$v,decreasing=T)[1:5]
      if (!all(is.na(indexes)))
                  return(filler(mydictionary[
                        drop_simple_sparse_array(trimatrix[i,j,])$i[indexes],1]))
            #      else return(findBest(y))
      
      return(as.character(mydictionary[1:5,1]))
}
findBest4U <-function(x) {
      
      #     return(mydictionary[which.max(as.vector(quadmatrix[which(mydictionary$label==x),
      #                                                      which(mydictionary$label==y),
      #                                              which(mydictionary$label==z),])),]) 
      x<-x[(length(x)-2):length(x)]
      i<-which(mydictionary$label==x[1])
      j<-which(mydictionary$label==x[2])
      k<-which(mydictionary$label==x[3])
      if (length(i) == 0) i<-Uindex
      if (length(j) == 0) j<-Uindex
      if (length(k) == 0) k<-Uindex
      indexes<-order(quadmatrix[i,j,k,]$v,decreasing=T)[1:5]
            if (!all(is.na(indexes)))
                  return(fillerfun(mydictionary[
                        drop_simple_sparse_array(quadmatrix[i,j,k,])$i[indexes],1],
                        findBest3U(x[2:3])))
      return(findBest3U(x[2:3]))
}
findBest4ood <-function(x) {
      #      return(mydictionary[which.max(as.vector(trimatrix[which(mydictionary$label==x),
      #                                                      which(mydictionary$label==y),])),]) 
      x<-x[(length(x)-2):length(x)]
      i<-which(mydictionary$label==x[1])
      j<-which(mydictionary$label==x[2])
      k<-which(mydictionary$label==x[3])
      if (length(i)>0 & length(j)>0 & length(k)>0) {
            indexes<-order(quadmatrix[i,j,k,]$v,decreasing=T)[1:5]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(fillerfun(mydictionary[
                        drop_simple_sparse_array(quadmatrix[i,j,k,])$i[indexes],1],
                        findBest3ood(x[2:3])))
            else return(findBest3ood(x[2:3]))
      }
      if (length(i)>0 & length(j)>0) {
            #            print("1 in")
            indexes<-order(quadmatrix[i,j,,]$v,decreasing=T)[1:20]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(fillerfun(mydictionary[unique(
                        drop_simple_sparse_array(quadmatrix[i,j,,])$i[indexes,2])[1:5],1],
                        findBest2(x[2])))
            else return(findBest2(x[2]))
      }
      if (length(i)>0 & length(k)>0) {
            #            print("1 in")
            indexes<-order(quadmatrix[i,,k,]$v,decreasing=T)[1:20]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(fillerfun(mydictionary[unique(
                        drop_simple_sparse_array(quadmatrix[i,,k,])$i[indexes,2])[1:5],1],
                        findBest2(x[3])))
            else return(findBest2(x[3]))
      }
      if (length(j)>0 & length(k)>0) {
            #            print("1 in")
            indexes<-order(quadmatrix[,j,k,]$v,decreasing=T)[1:20]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(fillerfun(mydictionary[unique(
                        drop_simple_sparse_array(quadmatrix[,j,k,])$i[indexes,2])[1:5],1],
                        findBest2(x[3])))
            else return(findBest2(x[3]))
      }
      if (length(i)>0) {
            indexes<-order(quadmatrix[i,,,]$v,decreasing=T)[1:20]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(filler(mydictionary[unique(
                        drop_simple_sparse_array(quadmatrix[i,,,])$i[indexes,3])[1:5],1]))
      }
      if (length(j)>0) {
            indexes<-order(quadmatrix[,j,,]$v,decreasing=T)[1:20]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(filler(mydictionary[unique(
                        drop_simple_sparse_array(quadmatrix[,j,,])$i[indexes,3])[1:5],1]))
      }
      if (length(k)>0) {
            indexes<-order(quadmatrix[,,k,]$v,decreasing=T)[1:20]
            if (length(indexes>0) & !all(is.na(indexes))) 
                  return(filler(mydictionary[unique(
                        drop_simple_sparse_array(quadmatrix[,,k,])$i[indexes,3])[1:5],1]))
      }
      return(mydictionary[1:5,1])
}
findBest4UnoRoll <-function(x) {
      
      #     return(mydictionary[which.max(as.vector(quadmatrix[which(mydictionary$label==x),
      #                                                      which(mydictionary$label==y),
      #                                              which(mydictionary$label==z),])),]) 
      x<-x[(length(x)-2):length(x)]
      i<-which(mydictionary$label==x[1])
      j<-which(mydictionary$label==x[2])
      k<-which(mydictionary$label==x[3])
      if (length(i) == 0) i<-Uindex
      if (length(j) == 0) j<-Uindex
      if (length(k) == 0) k<-Uindex
      indexes<-order(quadmatrix[i,j,k,]$v,decreasing=T)[1:5]
            if (!all(is.na(indexes)))
                  return(filler(mydictionary[
                        drop_simple_sparse_array(quadmatrix[i,j,k,])$i[indexes],1]))
      
      return(mydictionary[1:5,1])
}

if (FALSE) {
      ##deprecated
      findBest4oodi <-function(x) {
            #      return(mydictionary[which.max(as.vector(trimatrix[which(mydictionary$label==x),
            #                                                      which(mydictionary$label==y),])),]) 
            x<-x[(length(x)-2):length(x)]
            if (x[1] %in% mydictionary$label & x[2] %in% mydictionary$label & 
                x[3] %in% mydictionary$label) {      
                  #            print("in")
                  vec<-as.vector(quadmatrix[which(mydictionary$label==x[1]),
                                            which(mydictionary$label==x[2]),
                                            which(mydictionary$label==x[3]),])
                  #      print("4")
                  if (max(vec)>0) return(mydictionary[order(vec,decreasing=T)[1:5],])
                  else return(findBest3ood(x[2:3]))
            }
            if (x[1] %in% mydictionary$label & x[2] %in% mydictionary$label) {
                  #            print("1 in")
                  j<-which(mydictionary$label==x[1])
                  k<-which(mydictionary$label==x[2])
                  target<-max(quadmatrix[j,k,,])
                  for (i in 1:nrow(mydictionary)) {
                        if (max((quadmatrix[j,k,i,]))==target) {
                              vec<-as.vector(quadmatrix[j,k,i,])
                              if (max(vec)>0) return(mydictionary[order(vec,decreasing=T)[1:5],])
                        }
                        
                  }
                  
            }
            if (x[1] %in% mydictionary$label & x[3] %in% mydictionary$label) {
                  #            print("1 in")
                  j<-which(mydictionary$label==x[1])
                  k<-which(mydictionary$label==x[3])
                  target<-max(quadmatrix[j,,k,])
                  for (i in 1:nrow(mydictionary)) {
                        if (max((quadmatrix[j,i,k,]))==target) {
                              vec<-as.vector(quadmatrix[j,i,k,])
                              if (max(vec)>0) return(mydictionary[order(vec,decreasing=T)[1:5],])
                        }
                        
                  }
                  
            }
            if (x[2] %in% mydictionary$label & x[3] %in% mydictionary$label) {
                  #            print("1 in")
                  j<-which(mydictionary$label==x[2])
                  k<-which(mydictionary$label==x[3])
                  target<-max(quadmatrix[,j,k,])
                  for (i in 1:nrow(mydictionary)) {
                        if (max((quadmatrix[i,j,k,]))==target) {
                              vec<-as.vector(quadmatrix[i,j,k,])
                              if (max(vec)>0) return(mydictionary[order(vec,decreasing=T)[1:5],])
                        }
                        
                  }
                  
            }
            return(mydictionary[1:5,])
      }
}