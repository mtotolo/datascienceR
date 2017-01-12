## This function saves a png file with a 2x2 collection of plots 
## from the Individual household electric power consumption Data Set.
## as specified in the assignment text. The data must be in the working directory.

plot4 <- function() {
      
      # read the data file
      alldata <- read.table("household_power_consumption.txt",sep=";",
                            header=TRUE, na.strings="?", stringsAsFactors=FALSE)
      
      # select only the desired dates
      mydata <- subset(alldata,Date=="1/2/2007" | Date=="2/2/2007")
      
      # reformat date and time 
      mydata$datetime <- as.POSIXct(paste(mydata$Date, mydata$Time), 
                                    format="%d/%m/%Y %H:%M:%S")
      
      #create png file
      png("plot4.png",width = 480, height = 480)
      
      #setup 2x2 canvas
      par(mfcol=c(2,2))
      
      #draw first plot as in "plot2.R"
      with(mydata,plot(datetime,Global_active_power,type="l",xlab="",
                       ylab="Global Active Power"))
      
      #draw second plot and legend as in "plot3.R"
      with(mydata,plot(datetime,Sub_metering_1,type="l",col="black",xlab="",ylab=
                             "Energy sub metering"))
      with(mydata,points(datetime,Sub_metering_2,type="l",col="red"))
      with(mydata,points(datetime,Sub_metering_3,type="l",col="blue"))
      legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1",
                                    "Sub_metering_2","Sub_metering_3"),bty="n")
      
      #draw third plot
      with(mydata,plot(datetime,Voltage,type="l"))
      
      #draw last plot
      with(mydata,plot(datetime,Global_reactive_power,type="l"))
      
      #close file
      dev.off()
      
}