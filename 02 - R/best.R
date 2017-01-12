best <- function(state, outcome) {
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      ## Check that state and outcome are valid
      states<- unique(data[,7])
      if (!(state %in% states)) {stop("invalid state")}
      outcomes<-c("heart attack", "heart failure", "pneumonia")
      if (!(outcome %in% outcomes)) {stop("invalid outcome")}
      ## Set column to search
      if (outcome=="heart attack") {index<-11}
      else if (outcome=="heart failure") {index<-17}
      else {index<-23}
      suppressWarnings(data[,index]<-as.numeric(data[,index]))
      ## Extract subset with names and desired variable
      list <- data[data$State==state,c(2,index)]
      ## remove NAs
      list <- list[!is.na(list[,2]),]
      ## find best and sort
      bestlist<-sort(list[list[,2]==min(list[,2]),1])
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      bestlist[1]
}