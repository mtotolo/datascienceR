plot1 <- function() {
      # load data set
      NEI <- readRDS("summarySCC_PM25.rds")
      
      # calculate total emissions per year
      totalEmissions <- aggregate( Emissions ~ year,data=NEI, FUN=sum)
      
      # open png file
      png("plot1.png")
      
      # plot total emissions vs year
      with(totalEmissions,plot(year,Emissions,main="Total Emissions in the US"))
      
      # the trend is pretty clear, no need to plot an interpolation
      # but I'll do it anyway
      abline(lm(Emissions~year,totalEmissions))
      
      #close file
      dev.off()
}