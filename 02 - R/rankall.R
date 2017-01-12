rankall <- function(outcome, num="best") {
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      ## Check that outcome is valid
      states<- sort(unique(data[,7]))
      outcomes<-c("heart attack", "heart failure", "pneumonia")
      if (!(outcome %in% outcomes)) {stop("invalid outcome")}
      ## Set column to search
      if (outcome=="heart attack") {index<-11}
      else if (outcome=="heart failure") {index<-17}
      else {index<-23}
      suppressWarnings(data[,index]<-as.numeric(data[,index]))
      output<-data.frame(row.names=states)
      output$state<-states
      for (x in states) {
            ## Extract subset with names and desired variable
            list <- data[data$State==x,c(2,index)]
            ## remove NAs
            list <- list[!is.na(list[,2]),]
            ## sort
            rankedlist<-list[order(list[,2],list[,1]),]
            ## Choose index and return name
            if (num=="best") {output[x,"hospital"]<-rankedlist[1,1]}
            else if (num=="worst") {output[x,"hospital"]<-tail(rankedlist[,1],1)}
            else {output[x,"hospital"]<-rankedlist[num,1]}
      }
      output
}