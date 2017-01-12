plot4 <- function() {
     
      # load data set and classfication code
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      #from SCC, get the codes for coal combustion related sources
      
      #first, get combustion indexes from level one
      comb <- grep("comb",SCC$SCC.Level.One, ignore.case=TRUE)
      
      #then, get coal sources from level three
      coal <- grep("coal",SCC$SCC.Level.Three, ignore.case=TRUE)
      
      #finally, get the indexes from the intersection
      indexes<- SCC$SCC[intersect(comb,coal)]
      
      # calculate total emissions in the US by year for coal combustion
      # related sources
      CoalCombUS <- aggregate( Emissions ~ year,
                               data=subset(NEI,SCC %in% indexes),FUN=sum)
      
      #open png file
      png("plot4.png")
      
      #plot total emissions vs year
      with(CoalCombUS,plot(year,Emissions,
                           main="Coal Combustion Related Emissions in the US"))
      
      #plot a linear trend line
      abline(lm(Emissions~year,CoalCombUS))
      
      #close file
      dev.off()
}