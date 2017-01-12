lookup4<-function(input,vector) {
      a<- vector %>% filter(pre==input)
      return(a$post)
      
}