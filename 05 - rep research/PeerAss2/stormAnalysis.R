#Alldata <- read.csv("repdata_data_StormData.csv.bz2", stringsAsFactors = F)
data <- Alldata[Alldata$FATALITIES!=0 | Alldata$INJURIES!=0 | Alldata$PROPDMG!=0
                | Alldata$CROPDMG!=0,c("FATALITIES","INJURIES","EVTYPE","PROPDMG","PROPDMGEXP",
                                       "CROPDMG","CROPDMGEXP")]

#data <- read.csv("PopDamage.csv",stringsAsFactors=F)

data[grep("WARM|HEAT|HYPERTH",data$EVTYPE,ignore.case =T),]$EVTYPE <- "HEAT"
data[grep("FLOOD|FLD",data$EVTYPE,ignore.case =T),]$EVTYPE <- "FLOOD"
data[grep("STORM|TSTM|BLIZZARD",data$EVTYPE,ignore.case =T),]$EVTYPE <- "STORM"
data[grep("WIND",data$EVTYPE,ignore.case =T),]$EVTYPE <- "WIND"
data[grep("SNOW",data$EVTYPE,ignore.case =T),]$EVTYPE <- "SNOW"
data[grep("COLD|FREEZ|LOW TEMP|WINT|HYPOTH|ICE|ICY|FROST|GLAZE",data$EVTYPE,
          ignore.case =T),]$EVTYPE <- "COLD"
data[grep("HURRICANE|TYPHOON",data$EVTYPE,ignore.case =T),]$EVTYPE <- "HURRICANE"
#data[grep("ICE",data$EVTYPE,ignore.case =T),]$EVTYPE <- "ICE"
#data[grep("ICY",data$EVTYPE,ignore.case =T),]$EVTYPE <- "ICE"
#data[grep("FROST",data$EVTYPE,ignore.case =T),]$EVTYPE <- "ICE"
#data[grep("GLAZE",data$EVTYPE,ignore.case =T),]$EVTYPE <- "ICE"
data[grep("TORNADO",data$EVTYPE,ignore.case =T),]$EVTYPE <- "TORNADO"
data[grep("SLIDE",data$EVTYPE,ignore.case =T),]$EVTYPE <- "LANDSLIDE"
data[grep("FIRE",data$EVTYPE,ignore.case =T),]$EVTYPE <- "FIRE"
data[grep("WATER",data$EVTYPE,ignore.case =T),]$EVTYPE <- "WATER"
data[grep("WAVE|SURF|SWELL",data$EVTYPE,ignore.case =T),]$EVTYPE <- "WAVES"
data[grep("MARINE",data$EVTYPE,ignore.case =T),]$EVTYPE <- "MARINE ACCIDENT"
data[grep("DRY",data$EVTYPE,ignore.case =T),]$EVTYPE <- "DROUGHT"
data[grep("AVALA",data$EVTYPE,ignore.case =T),]$EVTYPE <- "AVALANCHE"
data[grep("DUST",data$EVTYPE,ignore.case =T),]$EVTYPE <- "DUST DEVIL"
data[grep("RAIN|PRECIP|SLEET",data$EVTYPE,ignore.case =T),]$EVTYPE <- "RAIN"
data[grep("HAIL",data$EVTYPE,ignore.case =T),]$EVTYPE <- "HAIL"
data[grep("SEA",data$EVTYPE,ignore.case =T),]$EVTYPE <- "SEA"
data[grep("LIGHTNING",data$EVTYPE,ignore.case =T),]$EVTYPE <- "LIGHTNING"
data[grep("RIP C",data$EVTYPE,ignore.case =T),]$EVTYPE <- "RIP CURRENT"
data[grep("FOG",data$EVTYPE,ignore.case =T),]$EVTYPE <- "FOG"

data$myCROPEXP <- 0
data$myCROPEXP[grep("k",data$CROPDMGEXP,ignore.case = T)] <- 3
data$myCROPEXP[grep("m",data$CROPDMGEXP,ignore.case = T)] <- 6
data$myCROPEXP[grep("b",data$CROPDMGEXP,ignore.case = T)] <- 9
data$myPROPEXP <- 0
data$myPROPEXP[grep("k",data$PROPDMGEXP,ignore.case = T)] <- 3
data$myPROPEXP[grep("m",data$PROPDMGEXP,ignore.case = T)] <- 6
data$myPROPEXP[grep("b",data$PROPDMGEXP,ignore.case = T)] <- 9

data$CropDamage <- data$CROPDMG * 10^(data$myCROPEXP-9)
data$PropertyDamage <- data$PROPDMG * 10^(data$myPROPEXP-9)


AggrData <- aggregate(cbind(INJURIES,FATALITIES,CropDamage,PropertyDamage)~EVTYPE,
                      data=data,FUN=sum)


AggrData <- AggrData[order(AggrData$FATALITIES,AggrData$INJURIES,
                               decreasing=T),]

plotdata<-matrix(c(head(AggrData$FATALITIES,5),head(AggrData$INJURIES/10,5)),
                 nrow=2,byrow=T)

barplot(plotdata,beside=T,col=c("darkblue","red"),names.arg=head(AggrData$EVTYPE,5),
        cex.names=0.8, las=2,legend=c("Fatalities","Injuries"),
        ylab="Number of Fatalities and Injuries/10")

AggrData <- AggrData[order(AggrData$PropertyDamage,AggrData$CropDamage,
                           decreasing=T),]

plotdata<-matrix(c(head(AggrData$PropertyDamage,5),head(AggrData$CropDamage*10,5)),
                 nrow=2,byrow=T)

barplot(plotdata,beside=T,col=c("darkblue","red"),names.arg=head(AggrData$EVTYPE,5),
        cex.names=0.8, las=2,legend=c("Property Damage","Crop Damage"),
        ylab="Property (B$) and Crop Damage (B$*10)")
