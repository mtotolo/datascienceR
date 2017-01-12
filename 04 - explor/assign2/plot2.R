plot2 <- function() {
      # load data set
      NEI <- readRDS("summarySCC_PM25.rds")
      
      # calculate total emissions in Baltimore per year
      totalBaltimore <- aggregate( Emissions ~ year,data=subset(NEI,fips=="24510"),
                                                                FUN=sum)
      
      # open png file
      png("plot2.png")
      
      # plot total emissions vs year
      with(totalBaltimore,plot(year,Emissions,main="Total Emissions in Baltimore"))
      
      # the trend is pretty clear, no need to plot an interpolation
      # but I'll do it anyway
      abline(lm(Emissions~year,totalBaltimore))
      
      #close file
      dev.off()
}