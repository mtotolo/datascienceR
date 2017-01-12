loadsaveFreq <- function(file) {
      data<-as.data.frame(read.csv(file,stringsAsFactors = F,nrows = 500000))
      data <- data %>% mutate(pre=unlist(strsplit(label," [^ ]*$")))
      data <- data %>% rowwise %>% mutate(post=gsub(paste0(pre," "),"",label))
      data <- data %>% select(c(pre,post,freq))
      write.csv(data,file=paste0("Freq",file),row.names = F)
      return(data)
}