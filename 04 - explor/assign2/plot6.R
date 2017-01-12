plot6 <- function() {
      #load ggplot2
      library(ggplot2)
      
      # load data set and classfication code
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      #get the motor vehicle indexes from level two
      indexes <- SCC$SCC[grep("veh",SCC$SCC.Level.Two, ignore.case=TRUE)]
      
      # calculate total emissions by year for vehicle
      # related sources in Baltimore and Los Angeles County
      vehicles <- aggregate( Emissions ~ year+fips,
                  data=subset(NEI,SCC %in% indexes & (fips=="24510"|fips=="06037")),
                  FUN=sum)
      
      # relabel site names
      vehicles$fips <- gsub("24510","Baltimore",vehicles$fips)
      vehicles$fips <- gsub("06037","Los Angeles County",vehicles$fips)
      
      # plot 2 facets of emissions vs year for the sites, with a linear model 
      # to show the trend
      qplot(year,Emissions,data=vehicles,facets=.~fips) + 
            stat_smooth(method="lm",se=FALSE) + 
            labs(title="Motor Vehicle Related Emissions")
      #save file
      ggsave("plot6.png")
      
}