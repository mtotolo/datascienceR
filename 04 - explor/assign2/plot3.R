plot3 <- function() {
      #load ggplot2
      library(ggplot2)
      
      # load data set
      NEI <- readRDS("summarySCC_PM25.rds")
      
      # calculate total emissions in Baltimore by year and type
      Baltimore <- aggregate( Emissions ~ year+type,data=subset(NEI,fips=="24510"),
                                                                FUN=sum)
      # plot 4 facets by type of emissions vs year, with a linear model 
      # to show the trend
      qplot(year,Emissions,data=Baltimore,facets=.~type) + 
            stat_smooth(method="lm",se=FALSE) + 
            labs(title="Emissions in Baltimore by type")
      
      #save file
      ggsave("plot3.png")
}