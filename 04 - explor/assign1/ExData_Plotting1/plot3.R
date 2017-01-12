## This function saves a png file with the plot of the 
## Sub metering 1,2,3 variables from the Individual household electric 
## power consumption Data Set. The data must be in the working directory.

plot3 <- function() {
      
      # read the data file
      alldata <- read.table("household_power_consumption.txt",sep=";",
                            header=TRUE, na.strings="?", stringsAsFactors=FALSE)
      
      # select only the desired dates
      mydata <- subset(alldata,Date=="1/2/2007" | Date=="2/2/2007")
      
      # reformat date and time 
      mydata$datetime <- as.POSIXct(paste(mydata$Date, mydata$Time), 
                                    format="%d/%m/%Y %H:%M:%S")
      
      #create png file
      png("plot3.png",width = 480, height = 480)
      
      #plot the three scatter plots with the desired parameters
      with(mydata,plot(datetime,Sub_metering_1,type="l",col="black",xlab="",ylab=
                 "Energy sub metering"))
      with(mydata,points(datetime,Sub_metering_2,type="l",col="red"))
      with(mydata,points(datetime,Sub_metering_3,type="l",col="blue"))
    
      #plot the legend
      legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1",
                  "Sub_metering_2","Sub_metering_3"))
      
      #close file
      dev.off()
      
}