plot5 <- function() {
     
      # load data set and classfication code
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      #get the motor vehicle indexes from level two
      indexes <- SCC$SCC[grep("veh",SCC$SCC.Level.Two, ignore.case=TRUE)]
      
      # calculate total emissions by year for vehicle
      # related sources in Baltimore
      vehicleBaltimore <- aggregate( Emissions ~ year,
                  data=subset(NEI,SCC %in% indexes & fips=="24510"), FUN=sum)
      
      #open png file
      png("plot5.png")
      
      #plot total emissions vs year
      with(vehicleBaltimore,plot(year,Emissions,
                           main="Motor Vehicle Related Emissions in Baltimore"))
      
      #plot a linear trend line
      abline(lm(Emissions~year,vehicleBaltimore))
      
      #close file
      dev.off()
}