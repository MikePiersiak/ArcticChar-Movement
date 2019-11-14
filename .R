setwd("/Users/PiersiakM/Desktop/ArcticNet Char Data/Nain Receivers")

##Required Packages
require(ggplot2)
require(ggmap)
require(plotrix)
require(sp)
require(data.table)

#Read in Telemetry files from Nain
{
Rec_1 <- read.csv("Rec_122386.csv")
  Rec_1$Habitat <- "Estuary"
Rec_2 <- read.csv("Rec_124358.csv")
  Rec_2$Habitat <- "Estuary"
Rec_3 <- read.csv("Rec_124408.csv")
  Rec_3$Habitat <- "Estuary"
Rec_4 <- read.csv("Rec_124881.csv")
  Rec_4$Habitat <- "Estuary"
Rec_5 <- read.csv("Rec_547201.csv")
  Rec_5$Habitat <- "Fjord"
Rec_6 <- read.csv("Rec_547207.csv")
  Rec_6$Habitat <- "Fjord"
Rec_7 <- read.csv("Rec_547209.csv")
  Rec_7$Habitat <- "Fjord"
Rec_8 <- read.csv("Rec_547210.csv")
  Rec_8$Habitat <- "Estuary"
Rec_9 <- read.csv("Rec_547211.csv")
  Rec_9$Habitat <- "Estuary"
Rec_10 <- read.csv("Rec_547212.csv")
  Rec_10$Habitat <- "Fjord"
Rec_11 <- read.csv("Rec_547215.csv")
  Rec_11$Habitat <- "Fjord"
Rec_12 <- read.csv("Rec_547216.csv")
  Rec_12$Habitat <- "Estuary"
Rec_13 <- read.csv("Rec_547221.csv")
  Rec_13$Habitat <- "Fjord"
Rec_14 <- read.csv("Rec_547225.csv")
  Rec_14$Habitat <- "Fjord"
Rec_15 <- read.csv("Rec_547226.csv")
  Rec_15$Habitat <- "Fjord"
Rec_16 <- read.csv("Rec_547227.csv")
  Rec_16$Habitat <- "Fjord"
Rec_17 <- read.csv("Rec_547228.csv")
  Rec_17$Habitat <- "Fjord"
Rec_18 <- read.csv("Rec_547229.csv")
  Rec_18$Habitat <- "Coast"
}
#a<-levels(Rec_18$Transmitter)
head(Rec_17)

Nain_Data <- rbind(Rec_1,Rec_2,Rec_3,Rec_4,Rec_5,Rec_6,Rec_7,Rec_8,Rec_9,
                   Rec_10,Rec_11,Rec_12,Rec_13,Rec_14,Rec_15,Rec_16,Rec_17,Rec_18)
Nain_Data$Location <- rep("Nain")
#Nain_Data$Habitat <- as.factor(Nain_Data$Habitat)

##want columns 1,3,4,5
Nain_Data <- Nain_Data[c(1,2,3,4,5,13,14)]
colnames(Nain_Data) <- c("time","Receiver","ID","x","y","Habitat","Location")

Nain_Data$time <- as.POSIXct(Nain_Data$time, format = "%m/%d/%Y %H:%M", tz = "UTC")

##remove synch tags + non-char tags
{
Nain_Char <- subset(Nain_Data, ! ID %in% c("A69-1601-61687",
                                           "A69-1601-61677",
                                           "A69-1601-61682",
                                           "A69-1601-61684",
                                           "A69-1601-61694",
                                           "A69-1601-61667",
                                           "A69-1601-61686",
                                           "A69-1601-61327",
                                           "A69-1601-61673",
                                           "A69-1601-61689",
                                           "A69-1601-61676",
                                           "A69-1601-61665",
                                           "A69-1601-61685",
                                           "A69-1601-61671",
                                           "A69-1601-61679",
                                           "A69-1601-61692",
                                           "A69-1601-61674",
                                           "A69-1601-61680",
                                           "A69-1601-61675",
                                           "A69-1602-15271",
                                           "A69-1602-15274",
                                           "A69-1601-61691",
                                           "A69-1601-61693",
                                           "A69-1601-61690"))
Nain_Char$ID <- droplevels(Nain_Char$ID)
}

##Create Column showing date of first marine detection for each fish
test <- strsplit(as.character(Nain_Char$time), "-")
##Creates COlumn with just year values, used to determine date of first marine entry IN 2019 because tagging in 2018 was at end of season prior
##to freshwater entry
Nain_Char$Year <- sapply(test, "[", 1) 
Nain_Char$Year <- as.factor(Nain_Char$Year)

Nain_Char2019 <- droplevels(subset(Nain_Char, Year=="2019"))
str(Nain_Char2019)

Marine_Entry <- NULL

for (i in levels(Nain_Char2019$ID)){
  
  Tag <- subset(Nain_Char2019, ID==i)
  Tag$Marine_Entry <- rep(Tag$time[1])
  Marine_Entry <- rbind(Marine_Entry, Tag[1,])
  
}

##Creates column with Jan 1 considered as start time
Marine_Entry$Start_Time <- rep("2019-01-01")
##Determines Number of days from Jan 1 to date of initial marine entry for each fish
Marine_Entry$Migration_Day <- as.integer(difftime(Marine_Entry$Marine_Entry, Marine_Entry$Start_Time, units = "days", tz = "UTC"))
View(Marine_Entry)

##Nain_Char <- Marine_Entry

##Creates a column labeled "New_ID" that identifies each individual using last 4 digits of tag ID
##Need this to merge data with Genetic RepUnit Data
NewID <- strsplit(as.character(Nain_Char$ID), "-")
Nain_Char$New_ID <- as.factor(sapply(NewID, "[" , 3))

setwd("/Users/PiersiakM/Desktop/ArcticNet Char Data")

##read in spreadsheet with genetic assignments for each fish
Genetic.Data <- read.csv("Char_Genetic_Assignments.csv")
head(Genetic.Data)

##Creates same column (New_ID) in RepUnit spreadsheet for merging
test <- strsplit(as.character(Genetic.Data$Individual),"2018")
Genetic.Data$New_ID <- as.integer(sapply(test, "[", 2))
Genetic.Data$Location <- as.factor(sapply(test, "[", 1))
Nain.genes <- droplevels(subset(Genetic.Data, Location == "NAI"))

str(Nain.genes)

Nain.genes <- Nain.genes[order(Nain.genes$New_ID, decreasing = FALSE),]
Nain_Char <- Nain_Char[order(Nain_Char$New_ID, decreasing = FALSE),]
View(Nain.genes)


levels(as.factor(Nain.genes$New_ID))
levels(Nain_Char$New_ID)

View(Nain_Char)

##Need to merge these two sheets around ID variable (New_ID), but factor levels don't match as we are missing a shitload of 
##Genetic Data (Assigned RepUnits) as well as receiver data










##Saglek Receivers
setwd("/Users/PiersiakM/Desktop/ArcticNet Char Data/Saglek Receivers")

{
Sag_1 <- read.csv("VR2W-108140.csv")
Sag_1$Habitat <- "Estuary"
Sag_2 <- read.csv("VR2W-109352.csv")
Sag_2$Habitat <- "Estuary"
Sag_3 <- read.csv("VR2W-122175.csv")
Sag_3$Habitat <- "Estuary"
Sag_4 <- read.csv("VR2W-122181.csv")
Sag_4$Habitat <- "Estuary"
Sag_5 <- read.csv("VR2W-124206.csv")
Sag_5$Habitat <- "Estuary"
}

Saglek_Data <- rbind(Sag_1,Sag_2,Sag_3,Sag_4,Sag_5)
Saglek_Data$Location <- rep("Saglek")
#str(Saglek_Data)

Saglek_Data <- Saglek_Data[c(1,2,3,4,5,13,14)]
colnames(Saglek_Data) <- c("time","Receiver","ID","x","y","Habitat","Location")
#head(Saglek_Data)
Saglek_Data$time <- as.POSIXct(Saglek_Data$time, format = "%m/%d/%Y %H:%M", tz = "UTC")

##this loop determines # of individual receivers each tag was detected at

Saglek_Char <- subset(Saglek_Data, ! ID %in% c("A69-9006-2428",
                                               "A69-9006-2419"))
levels(Saglek_Char$ID)
Saglek_Char$ID <- droplevels(Saglek_Char$ID)
Saglek_Char$Location <- as.factor(Saglek_Char$Location)

Saglek.plot <- ggplot(Saglek_Char, aes(x=time, y=ID, color = Receiver))
Saglek.plot + geom_point() + theme(legend.position = "bottom") + labs(title = "Saglek Char")

Nain.plot <- ggplot(Nain_Char, aes(x=time, y=ID, color = Receiver))
Nain.plot + geom_point() + theme(legend.position = "bottom") + labs(title = "Nain Char")

##Combine Saglek and Nain sheets
Char_tags <- rbind(Nain_Char, Saglek_Char)
str(Char_tags)

Omnibus.plot <- ggplot(Char_tags, aes(x=time, y=ID, color = Receiver))
Omnibus.plot + geom_point() + theme(legend.position = "bottom") + facet_wrap(~Location)

Char_tags$Receiver <- droplevels(Char_tags$Receiver)
#levels(Char_tags$Receiver)

rec.locs <- NULL

for (i in levels(Char_tags$Receiver)){
  
  a <- subset(Char_tags, Receiver==i)
  Lat <- a$x[1]
  Lon <- a$y[1]
  loc.frame <- cbind(i,Lat,Lon, a$Location[1], a$Habitat[1])
  colnames(loc.frame) <- c("ID","y","x","Location","Habitat")
  rec.locs <- as.data.frame(rbind(rec.locs, loc.frame))
}

##removed because it had no detections
rec.locs <- subset(rec.locs, ! ID %in% c("VR2AR-547226"))
rec.locs$x <- as.integer(levels(rec.locs$x))
rec.locs$y <- as.integer(levels(rec.locs$y))
#head(rec.locs)
#View(rec.locs)

utmcoor<-SpatialPoints(cbind(rec.locs$x,rec.locs$y), proj4string=CRS("+proj=utm +zone=20V"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
rec.locs$Lat <- coordinates(longlatcoor)[,2]
rec.locs$Lon <- coordinates(longlatcoor)[,1]

nain.locs <- subset(rec.locs, Location=="Nain")
head(nain.locs)

qmplot(Lon, Lat, data = nain.locs, colour = nain.locs$Habitat, size = I(3), darken = .3)


###this loop determines the number of unique tag IDs detected at each receiver
sheet1 <- NULL

for (i in levels(Char_tags$Receiver)){
  
  Rec <- subset(Char_tags, Receiver==i)
  tags <- nlevels(droplevels(Rec$ID))
  sheet <- cbind(i,tags, Rec$x[1], Rec$y[1], Rec$Location[1],Rec$Habitat[1])
  sheet1 <- data.frame(rbind(sheet1, sheet))
}

colnames(sheet1) <- c("ID",
                      "nDetections",
                      "Lat",
                      "Lon",
                      "Location",
                      "Habitat")
sheet1$nDetections <- as.integer(sheet1$nDetections)
head(sheet1)

det.plot <- ggplot(sheet1, aes(x=Habitat, y=nDetections, color = Location))
det.plot + geom_point(position = (position_dodge(width = 0.2))) + theme(axis.text.x = element_text(angle = 90,vjust = .6))

sheet1$Lat <- as.integer(as.character(sheet1$Lat))
sheet1$Lon <- as.integer(as.character(sheet1$Lon))

utmcoor<-SpatialPoints(cbind(sheet1$Lon,sheet1$Lat), proj4string=CRS("+proj=utm +zone=20V"))
longlatcoor1<-spTransform(utmcoor,CRS("+proj=longlat"))
sheet1$Lat <- coordinates(longlatcoor1)[,2]
sheet1$Lon <- coordinates(longlatcoor1)[,1]
sheet1$nDetections <- as.integer(as.character(sheet1$nDetections))

qmplot(Lon, Lat, data = sheet1, colour = sheet1$Habitat, size = sheet1$nDetections, darken = .3) + 
  labs(size="Unique Fish IDs", title = "Arctic Char Detections in Saglek and Nain Bay") #+ theme(legend.position = "bottom")



##Classify receivers by habitat type/location (i.e. estuary, inner coast, outer coast)


##The below plot is wrong, its using number of unique tags detected at each hab type, need to make it 
  ##proportion of total detections for individual fish
  ##needs to be a nested for-loop first delineating a tag
  ##then looking at each receiver and seeing how many times that tag appeared there
  ##relative to the total number of detections


final.frame <- NULL

for (i in levels(Char_tags$ID)){
  
  Tag <- subset(Char_tags, ID==i)
  #View(Tag)
  Tag$Receiver <- droplevels(Tag$Receiver)
  
  Det.frame <- NULL
  for (m in levels(Tag$Receiver)){
    Rec <- subset(Tag, Receiver==m)
    nDet <- nrow(Rec)
    detProportion <- nDet/(nrow(Tag))
    nDetections <- cbind(i,Rec$Location[1],Rec$Habitat[1],m,nDet,detProportion)
    Det.frame <- data.frame(rbind(Det.frame, nDetections))
    
    }
  #View(Det.frame)
  
  final.frame <- rbind(final.frame, Det.frame)
  
}

colnames(final.frame) <- c("ID","Location","Habitat","Receiver","nDetections","PercentTotal")
final.frame$nDetections <- as.numeric(as.character(final.frame$nDetections))
final.frame$PercentTotal <- as.numeric(as.character(final.frame$PercentTotal))

det.plot <- ggplot(final.frame, aes(x=ID, y=PercentTotal, color = Habitat, shape = Location))
det.plot + geom_point() + theme(axis.text.x = element_text(angle = 90,vjust = .6), legend.position = "none")

head(final.frame)

##this plot is wrong, no data for each habitat type at both locations (gives 2 error messages regarding 
##inner fjord and outer coast from Saglek due to lack of values from these habitat types)
final.frame <- subset(final.frame, Location=="Nain")

a <- mean(final.frame$nDetections[which(final.frame$Habitat=="Estuary")])
b <- mean(final.frame$nDetections[which(final.frame$Habitat=="Fjord")])  
c <- mean(final.frame$nDetections[which(final.frame$Habitat=="Coast")])

d <- std.error(final.frame$nDetections[which(final.frame$Habitat=="Estuary")])
e <- std.error(final.frame$nDetections[which(final.frame$Habitat=="Fjord")])
f <- std.error(final.frame$nDetections[which(final.frame$Habitat=="Coast")])

plot.frame = data.frame(
  Habitat = c("Estuary",
           "Fjord",
           "Coast"), 
  nDetections = c(a,b,c), 
  se = c(d,e,f))

DetectionPlot = ggplot(plot.frame, aes(x=Habitat, y=nDetections))
DetectionPlot + geom_point() + geom_errorbar(aes(ymax = nDetections + se, 
                                                 ymin = nDetections - se, 
                                                 width = 0.2)) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45,vjust = .6)) + 
  labs(y="Detections by Location")

##calculate time elapsed between detections to det. total time in estuary or given habitat type
##directionality of movement to get river or aquatic habitat use patterns (i.e. river entry and exit)
##calculate total nDetections for each fish and standardize nDetection plot above (i.e. each point will
  ##represent a proportion of the respective individuals total detections)
##timing of first entry in to salt

##Working with Nain fish because of missing recs from Saglek

Nain_Fish <- subset(final.frame, Location=="Nain")
Nain_Fish$Location <- droplevels(Nain_Fish$Location)

plot.frame = data.frame(
  Habitat = c("Estuary",
              "Fjord",
              "Coast"), 
  nDetections = c(a,b,c), 
  se = c(d,e,f))
Nain_Plot <- ggplot(Nain_Fish, aes(x=ID, y=PercentTotal, color = Habitat))
Nain_Plot + geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = .6))


# Time Elapsed Between Concurrent Detections ------------------------------

##calculate time elapsed between detections to det. total time in estuary or given habitat type

Char_tags <- rbind(Nain_Char, Saglek_Char)
Char_tags$time <- as.factor(Char_tags$time)

Char_tags <- data.table(Char_tags)
str(Char_tags)

test <- Char_tags[ , shift(time, type = "lag"), by = Receiver]
head(test)



##this loop determines the time interval between concurrent detections at the same receiver. ##likely problems
##in terms of time series where r is looking at all detections from a given tag that occurred at each receiver, 
##regardless of whether the fish moved to a different location, was detected there, and came back to the initial
##receiver. NEED TO FIX THIS

time.sheet <- NULL

for (i in levels(Char_tags$ID)){
  
  Tag <- data.table(subset(Char_tags, ID==i))
  test.col <- Tag[ , shift(time, type = "lag"), by = Receiver]
  colnames(test.col) <- c("Receiver","lag.time")
  Tag <- cbind(Tag, test.col$lag.time)
  time.sheet <- rbind(time.sheet, Tag)


}


time.sheet <- data.frame(time.sheet)
names(time.sheet)[8]<-"time2"
time.sheet$time <- as.POSIXct(time.sheet$time, format = "%Y-%m-%d %H:%M", tz = "UTC")
time.sheet$time2 <- as.POSIXct(time.sheet$time2, format = "%Y-%m-%d %H:%M", tz = "UTC")

time.sheet$TimeDiff <- as.integer(difftime(time.sheet$time, time.sheet$time2, units = "mins", tz = "UTC"))
View(time.sheet)
time.sheet$TimeDiff.hrs <- as.numeric(time.sheet$TimeDiff/60)

test.fish <- subset(Char_tags, ID=="A69-1602-4380")
View(test.fish)
