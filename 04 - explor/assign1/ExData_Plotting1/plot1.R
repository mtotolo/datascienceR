## This function saves a png file with the histogram of the 
## Global Active Power variable from the Individual household electric 
## power consumption Data Set. The data must be in the working directory.

plot1 <- function() {
      
      # read the data file
      alldata <- read.table("household_power_consumption.txt",sep=";",
                            header=TRUE, na.strings="?", stringsAsFactors=FALSE)
      
      # select only the desired dates
      mydata <- subset(alldata,Date=="1/2/2007" | Date=="2/2/2007")
      
      # reformat date and time 
      mydata$datetime <- as.POSIXct(paste(mydata$Date, mydata$Time), 
                                    format="%d/%m/%Y %H:%M:%S")
      
      #create png file
      png("plot1.png",width = 480, height = 480)
      
      #plot the histogram with the desired parameters
      hist(mydata$Global_active_power, col="red",main="Global Active Power", 
           xlab="Global Active Power (kilowatts)")
      
      #close file
      dev.off()
      
}