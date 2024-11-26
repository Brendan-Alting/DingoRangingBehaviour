#Script 0.1 Collar data, plotting the study area, and illustrating territorial zones. 
#this will be the basis of our analysis- divide the study area into different territorial boundaries. We'll plot to illustrate this, and seperate the cameras into different zones for each species. 


library(sf)
library(adehabitatHR)
library(rgdal)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggmap)


####First lets do 2021/2022
Andy22 <- read.csv(file = "Raw Data/Collar1/UOF1501.csv", header = T)
SWY22 <- read.csv(file = "Raw Data/Collar1/WTF2204.csv", header =T)
Cathy22 <- read.csv(file = "Raw Data/Collar1/SLF2003.csv", header = T)
RD22 <- read.csv(file = "Raw Data/Collar1/UOM2002.csv", header = T)
Tekka22 <- read.csv(file = "Raw Data/Collar1/YGM2102.csv",header=T)


# Clean column names for Tekka22
colnames(Tekka22)[colnames(Tekka22) == "Julianday"] <- "Day"
colnames(Tekka22)[colnames(Tekka22) == "NumSats"] <- "Num_of_sats"

# Get fix times into proper times for all new individuals
Andy22$DateTime <- as.POSIXct(paste(2000 + Andy22$Year, "-", Andy22$Day, " ", Andy22$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
SWY22$DateTime <- as.POSIXct(paste(2000 + SWY22$Year, "-", SWY22$Day, " ", SWY22$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
Cathy22$DateTime <- as.POSIXct(paste(2000 + Cathy22$Year, "-", Cathy22$Day, " ", Cathy22$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
RD22$DateTime <- as.POSIXct(paste(2000 + RD22$Year, "-", RD22$Day, " ", RD22$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
Tekka22$DateTime <- as.POSIXct(paste(2000 + Tekka22$Year, "-", Tekka22$Day, " ", Tekka22$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

# Remove clear outliers for new individuals
CleanAndy22 <- Andy22[Andy22$Latitude < -30, ]
CleanAndy22 <- Andy22[Andy22$Longitude < 152.22, ]

CleanSWY22 <- SWY22[SWY22$Latitude < -32.58, ]
CleanCathy22 <- Cathy22[Cathy22$Latitude < -30, ]
CleanRD22 <- RD22[RD22$Latitude < -30, ]
CleanTekka22 <- Tekka22[Tekka22$Latitude < -30, ]

# Remove hdop >2 for new individuals
CleanAndy22 <- CleanAndy22[CleanAndy22$Hdop < 2, ]#DOnt run- all lost
CleanCathy22 <- CleanCathy22[CleanCathy22$Hdop < 2, ]
CleanSWY22 <- CleanSWY22[CleanSWY22$Hdop <2,]
CleanRD22 <- CleanRD22[CleanRD22$Hdop < 2, ]
CleanTekka22 <- CleanTekka22[CleanTekka22$Hdop < 2, ]

# Plot data for new individuals
plot(CleanAndy22$Latitude ~ CleanAndy22$Longitude)
plot(CleanSWY22$Latitude ~ CleanSWY22$Longitude)
plot(CleanCathy22$Latitude ~ CleanCathy22$Longitude)
plot(CleanRD22$Latitude ~ CleanRD22$Longitude)
plot(CleanTekka22$Latitude ~ CleanTekka22$Longitude)

# Selecting date range preferred. If have, use.
start_date22 <- as.POSIXct("2021-08-01 00:00:00")
end_date22 <- as.POSIXct("2021-12-01 00:00:00")

Andy22Cleandates <- CleanAndy22[CleanAndy22$DateTime >= start_date22 & CleanAndy22$DateTime <= end_date22, ]
SWY22Cleandates<- CleanSWY22[CleanSWY22$DateTime >= "2023-04-29 00:00:00" & CleanSWY22$DateTime <= "2023-12-01 00:00:00", ]
Cathy22Cleandates <- CleanCathy22[CleanCathy22$DateTime >= start_date22 & CleanCathy22$DateTime <= "2021-11-01 00:00:00", ]  # Cathy use pre-dispersal period
Cathy22Cleandatesplot <- CleanCathy22[CleanCathy22$DateTime >= "2021-04-01 00:00:00" & CleanCathy22$DateTime <= "2021-11-01 00:00:00", ]  # Cathy use pre-dispersal period

RD22Cleandates <- CleanRD22[CleanRD22$DateTime >= start_date22 & CleanRD22$DateTime <= end_date22, ]
Tekka22Cleandates <- CleanTekka22[CleanTekka22$DateTime >= "2022-04-01 00:00:00" & CleanTekka22$DateTime <= "2022-08-01 00:00:00", ]  # Tekka use previous year - and pre-dispersal
Tekka22Cleandatesplot <- CleanTekka22[CleanTekka22$DateTime >= "2022-04-01 00:00:00" & CleanTekka22$DateTime <= "2022-08-01 00:00:00", ]  # Tekka use previous year


# Remove rows with NA Latitude
Andy22Cleandates <- Andy22Cleandates[!is.na(Andy22Cleandates$Latitude), ]
SWY22Cleandates <- SWY22Cleandates[!is.na(SWY22Cleandates$Latitude), ]
Cathy22Cleandates <- Cathy22Cleandates[!is.na(Cathy22Cleandates$Latitude), ]
Cathy22Cleandatesplot <- Cathy22Cleandatesplot[!is.na(Cathy22Cleandatesplot$Latitude), ]
RD22Cleandates <- RD22Cleandates[!is.na(RD22Cleandates$Latitude), ]
Tekka22Cleandates <- Tekka22Cleandates[!is.na(Tekka22Cleandates$Latitude), ]
Tekka22Cleandatesplot <- Tekka22Cleandatesplot[!is.na(Tekka22Cleandatesplot$Latitude),]

# Add Individual column
Andy22Cleandates$Individual <- "Andy22"
SWY22Cleandates$Individual <- "SWY22"
Cathy22Cleandates$Individual <- "Cathy22"
Cathy22Cleandatesplot$Individual <- "Cathy22"
RD22Cleandates$Individual <- "RD22"
Tekka22Cleandates$Individual <- "Tekka22"
Tekka22Cleandatesplot$Individual <- "Tekka22"

# Combine all cleaned data
CleanAllTogether22 <- rbind(
  Andy22Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  SWY22Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  Cathy22Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  RD22Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  Tekka22Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")]
)


# Convert to sf object
combined_sf22 <- st_as_sf(CleanAllTogether22, coords = c("Longitude", "Latitude"), crs = st_crs(4326))



# Create sf objects for new individuals
Andy22points_sf <- combined_sf22[combined_sf22$Individual == "Andy22", ]
SWY22points_sf <- combined_sf22[combined_sf22$Individual == "SWY22", ]
Cathy22points_sf <- combined_sf22[combined_sf22$Individual == "Cathy22", ]
RD22points_sf <- combined_sf22[combined_sf22$Individual == "RD22", ]
Tekka22points_sf <- combined_sf22[combined_sf22$Individual == "Tekka22", ]

# Take random sample of 2000 points for each new individual-or dont???? %>%Sample_n(2000) if want
random_Andy22_sf <- Andy22points_sf%>%sample_n(2000)
random_SWY22_sf <- SWY22points_sf%>%sample_n(2000)
random_cathy22_sf <- Cathy22points_sf%>%sample_n(2000)
random_rd22_sf <- RD22points_sf%>%sample_n(2000)
random_tekka22_sf <- Tekka22points_sf%>%sample_n(2000)

# Transform into UTM and assign spatial for these functions
random_Andy22_sp <- as(random_Andy22_sf, "Spatial")
random_SWY22_sp <- as(random_SWY22_sf, "Spatial")
random_cathy22_sp <- as(random_cathy22_sf, "Spatial")
random_rd22_sp <- as(random_rd22_sf, "Spatial")
random_tekka22_sp <- as(random_tekka22_sf, "Spatial")

Andy22PointsUTM <- spTransform(random_Andy22_sp, CRS("+init=epsg:28356"))
SWY22PointsUTM <- spTransform(random_SWY22_sp, CRS("+init=epsg:28356"))
Cathy22PointsUTM <- spTransform(random_cathy22_sp, CRS("+init=epsg:28356"))
RD22PointsUTM <- spTransform(random_rd22_sp, CRS("+init=epsg:28356"))
Tekka22PointsUTM <- spTransform(random_tekka22_sp, CRS("+init=epsg:28356"))


#Data is cleaned- now we are going to estimate the 50% MCPs



#Calculate MCP----#Ignore warnings
Andy22Core <- mcp(Andy22PointsUTM, percent = 50)
SWY22Core <- mcp(SWY22PointsUTM, percent =50)
Cathy22Core <- mcp(Cathy22PointsUTM, percent = 50)
RD22Core <- mcp(RD22PointsUTM, percent = 50)
Tekka22Core <- mcp(Tekka22PointsUTM, percent =50)

#Calculate MCP----#Ignore warnings
Andy22Periph <- mcp(Andy22PointsUTM, percent = 95)
SWY22Periph <- mcp(SWY22PointsUTM, percent =95)
Cathy22Periph <- mcp(Cathy22PointsUTM, percent = 96)
RD22Periph <- mcp(RD22PointsUTM, percent = 95)
Tekka22Periph <- mcp(Tekka22PointsUTM, percent =95)


#Calculate Kernel utilisation distributions----#Ignore warnings
Andy22KUD <- kernelUD(Andy22PointsUTM, h = "href")
SWY22KUD <- kernelUD(SWY22PointsUTM, h = "href")
Cathy22KUD <- kernelUD(Cathy22PointsUTM, h = "href")
RD22KUD <- kernelUD(RD22PointsUTM, h ="href")
Tekka22KUD <- kernelUD(Tekka22PointsUTM, h = "href")




#Calculate KDES####
Andy22CoreKDE <- getverticeshr(Andy22KUD, percent = 50)
SWY22CoreKDE <- getverticeshr(SWY22KUD, percent =50)
Cathy22CoreKDE <- getverticeshr(Cathy22KUD, percent = 50)
RD22CoreKDE <- getverticeshr(RD22KUD, percent = 50)
Tekka22CoreKDE <- getverticeshr(Tekka22KUD, percent =50)

#Calculate KDES
Andy22PeriphKDE <- getverticeshr(Andy22KUD, percent = 95)
SWY22PeriphKDE <- getverticeshr(SWY22KUD, percent =95)
Cathy22PeriphKDE <- getverticeshr(Cathy22KUD, percent = 95)
RD22PeriphKDE <- getverticeshr(RD22KUD, percent = 95)
Tekka22PeriphKDE <- getverticeshr(Tekka22KUD, percent =95)

#Give them names 

Andy22Core$CollarPack <- "JB"
SWY22Core$CollarPack <- "WT"
Cathy22Core$CollarPack <- "SL"
RD22Core$CollarPack <- "MB"
Tekka22Core$CollarPack <- "YG"

Andy22Periph$CollarPack <- "JB"
SWY22Periph$CollarPack <- "WT"
Cathy22Periph$CollarPack <- "SL"
RD22Periph$CollarPack <- "MB"
Tekka22Periph$CollarPack <- "YG"


Andy22CoreKDE$CollarPack <- "JB"
SWY22CoreKDE$CollarPack <- "WT"
Cathy22CoreKDE$CollarPack <- "SL"
RD22CoreKDE$CollarPack <- "MB"
Tekka22CoreKDE$CollarPack <- "YG"

Andy22PeriphKDE$CollarPack <- "JB"
SWY22PeriphKDE$CollarPack <- "WT"
Cathy22PeriphKDE$CollarPack <- "SL"
RD22PeriphKDE$CollarPack <- "MB"
Tekka22PeriphKDE$CollarPack <- "YG"

#Add in name for MCP

Andy22Core$TerritoryZone <- "Core"
SWY22Core$TerritoryZone <- "Core"
Cathy22Core$TerritoryZone <- "Core"
RD22Core$TerritoryZone <- "Core"
Tekka22Core$TerritoryZone <- "Core"

Andy22Periph$TerritoryZone <- "Periph"
SWY22Periph$TerritoryZone <- "Periph"
Cathy22Periph$TerritoryZone <- "Periph"
RD22Periph$TerritoryZone <- "Periph"
Tekka22Periph$TerritoryZone <- "Periph"

Andy22CoreKDE$TerritoryZone <- "Core"
SWY22CoreKDE$TerritoryZone <- "Core"
Cathy22CoreKDE$TerritoryZone <- "Core"
RD22CoreKDE$TerritoryZone <- "Core"
Tekka22CoreKDE$TerritoryZone <- "Core"

Andy22PeriphKDE$TerritoryZone <- "Periph"
SWY22PeriphKDE$TerritoryZone <- "Periph"
Cathy22PeriphKDE$TerritoryZone <- "Periph"
RD22PeriphKDE$TerritoryZone <- "Periph"
Tekka22PeriphKDE$TerritoryZone <- "Periph"




#merge 

AllCore22 <- rbind(Andy22Core, SWY22Core, Cathy22Core, RD22Core, Tekka22Core)
AllAll22 <- rbind(Andy22Core, SWY22Core, Cathy22Core, RD22Core, Tekka22Core, Andy22Periph, SWY22Periph, Cathy22Periph, RD22Periph, Tekka22Periph)
AllCore22KDE <- rbind(Andy22CoreKDE, SWY22CoreKDE,Cathy22CoreKDE, RD22CoreKDE,Tekka22CoreKDE)
AllAll22KDE <- rbind(Andy22CoreKDE, SWY22CoreKDE,Cathy22CoreKDE, RD22CoreKDE,Tekka22CoreKDE,Andy22PeriphKDE, SWY22PeriphKDE,Cathy22PeriphKDE, RD22PeriphKDE,Tekka22PeriphKDE)


AllCoresf22 <- st_as_sf(AllCore22, crs =st_crs(32756))
AllCoresf22 <- st_transform(AllCoresf22, crs = 32756)

AllAllsf22 <- st_as_sf(AllAll22,crs = st_crs(32756))
AllAllsf22 <- st_transform(AllAllsf22, crs = 32756)

AllCoresf22kde <- st_as_sf(AllCore22KDE, crs = st_crs(32756))
AllCoresf22kde <- st_transform(AllCoresf22kde, crs= 32756)

AllAllsf22kde <- st_as_sf(AllAll22KDE, crs = st_crs(32756))
AllAllsf22kde <- st_transform(AllAllsf22kde, crs= 32756)


plot(AllAllsf22)




#Then repeat for 2022/2023:::

#Step one is to clean all data. 


###Read in Collar Data for individuals
Andy <- read.csv(file = "Raw Data/Collar2/UOM2008.csv", header = T)  
SWY <- read.csv(file = "Raw Data/Collar2/WTF2204.csv", header= T)
Tekka <- read.csv(file = "Raw Data/Collar2/YGM2102.csv", header= T)
Bombah <- read.csv(file = "Raw Data/Collar2/UOF1801.csv", header = T)
RedDog <- read.csv(file = "Raw Data/Collar2/SLF1501.csv", header = T)

###starting with data cleaning
colnames(Tekka)[colnames(Tekka) == "Julianday"] <- "Day"
colnames(Tekka)[colnames(Tekka) == "NumSats"] <- "Num_of_sats"
#get Tekka's fix times into proper times. 
Tekka$DateTime <- as.POSIXct(paste(2000 + Tekka$Year, "-", Tekka$Day, " ", Tekka$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

#get SWY's fix times into proper times. 
SWY$DateTime <- as.POSIXct(paste(2000 + SWY$Year, "-", SWY$Day, " ", SWY$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

#get Andy's fix times into proper times. 
Andy$DateTime <- as.POSIXct(paste(2000 + Andy$Year, "-", Andy$Day, " ", Andy$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")


#get Bombah's fix times into proper times. 
Bombah$DateTime <- as.POSIXct(paste(2000 + Bombah$Year, "-", Bombah$Day, " ", Bombah$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

#get Sock's fix times into proper times. 
RedDog$DateTime <- as.POSIXct(paste(2000 + RedDog$Year, "-", RedDog$Day, " ", RedDog$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")


##remove clear outliers
CleanAndy <- Andy[(Andy$Latitude < -30), ] 
CleanSWY <- SWY[SWY$Latitude < -32.58, ]
CleanBombah <- Bombah[Bombah$Latitude < -31, ]
CleanTekka <- Tekka[Tekka$Latitude < -30, ]
CleanRedDog <- RedDog[RedDog$Latitude < -30, ]

#remove hdop <2
CleanAndy <- CleanAndy[(CleanAndy$Hdop <2),]
CleanSWY <- CleanSWY[(CleanSWY$Hdop <2),]
CleanBombah <- CleanBombah[(CleanBombah$Hdop<2),]
CleanTekka <- CleanTekka[(CleanTekka$Hdop<2),]
CleanRedDog <- CleanRedDog[(CleanRedDog$Hdop<2),]

plot(CleanAndy$Latitude ~ CleanAndy$Longitude)
plot(CleanSWY$Latitude ~ CleanSWY$Longitude)
plot(CleanBombah$Latitude ~ CleanBombah$Longitude)
plot(CleanTekka$Latitude ~ CleanTekka$Longitude)
plot(CleanRedDog$Latitude ~ CleanRedDog$Longitude)

#selecting date range preferred. If have, use. 
start_date <- as.POSIXct("2022-08-01 00:00:00")
end_date <- as.POSIXct("2022-12-01 00:00:00")

AndyCleandates<- CleanAndy[CleanAndy$DateTime >= start_date & CleanAndy$DateTime <= end_date, ] #good for andy
SWYCleandates<- CleanSWY[CleanSWY$DateTime >= "2023-04-29 00:00:00" & CleanSWY$DateTime <= "2023-12-01 00:00:00", ]
BombahCleandates<- CleanBombah[CleanBombah$DateTime >= start_date & CleanBombah$DateTime <= "2022-11-01 00:00:00", ] #Good for bombah
TekkaCleandates <- CleanTekka[CleanTekka$DateTime >= "2022-04-01 00:00:00" & CleanTekka$DateTime <= "2022-08-01 00:00:00", ]  ##pre dispersal
RedDogCleandates <- CleanRedDog[CleanRedDog$DateTime >= "2022-05-15 00:00:00" & CleanRedDog$DateTime <= end_date, ]


BombahCleandates <- BombahCleandates[-which(is.na(BombahCleandates$Latitude)),]
SWYCleandates <- SWYCleandates[-which(is.na(SWYCleandates$Latitude)),]
AndyCleandates <- AndyCleandates[-which(is.na(AndyCleandates$Latitude)),]
TekkaCleandates <- TekkaCleandates[-which(is.na(TekkaCleandates$Latitude)),]
RedDogCleandates <- RedDogCleandates[-which(is.na(RedDogCleandates$Latitude)),]###DONT RUN, NO NAs





AndyCleandates$Individual <- "Andy"
SWYCleandates$Individual <- "SWY"
BombahCleandates$Individual <- "Bombah"
TekkaCleandates$Individual <- "Tekka"
RedDogCleandates$Individual <- "RedDog"

CleanAllTogether <- rbind(
  AndyCleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  SWYCleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  BombahCleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  TekkaCleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  RedDogCleandates[, c("DateTime", "Individual", "Latitude", "Longitude")])

CleanAllTogether <- CleanAllTogether%>%filter(!Latitude == "NA")

##plotting all together
ggplot(CleanAllTogether, aes(x = Longitude, y = Latitude, color = Individual)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "GPS Coordinates with Individual Coloring") +
  theme_minimal()
#Data looks pretty nice. 

#make sf object
combined_sf <- st_as_sf(CleanAllTogether, coords = c("Longitude", "Latitude"), crs = st_crs(4326))
#make each dataframe sf object too 

Andypoints_sf<- combined_sf[combined_sf$Individual == "Andy",]
SWYpoints_sf<- combined_sf[combined_sf$Individual == "SWY",] 
Bombahpoints_sf <- combined_sf[combined_sf$Individual == "Bombah",]
Tekkapoints_sf <- combined_sf[combined_sf$Individual == "Tekka",]
RedDogpoints_sf <- combined_sf[combined_sf$Individual == "RedDog",]


#Take random sample of 2000 points for each (if have)

random_andy_sf <- Andypoints_sf%>% sample_n(2000)
random_swy_sf <- SWYpoints_sf %>% sample_n(2000)
random_bombah_sf <- Bombahpoints_sf %>% sample_n(2000)
random_tekka_sf <- Tekkapoints_sf %>% sample_n(2000)
random_RedDog_sf <- RedDogpoints_sf %>% sample_n(2000)




#transform into utm and assign spatial for these functions.... 

random_andy_sp <- as(random_andy_sf, "Spatial")
random_SWYpoints_sp <- as(random_swy_sf, "Spatial")
random_Bombahpoints_sp <- as(random_bombah_sf, "Spatial")
random_Tekkapoints_sp <- as(random_tekka_sf, "Spatial")
random_RedDogpoints_sp <- as(random_RedDog_sf, "Spatial")

AndyPointsUTM <- spTransform(random_andy_sp, CRS("+init=epsg:28356"))
SWYPointsUTM <- spTransform(random_SWYpoints_sp, CRS("+init=epsg:28356"))
BombahPointsUTM <- spTransform(random_Bombahpoints_sp, CRS("+init=epsg:28356"))
TekkaPointsUTM <- spTransform(random_Tekkapoints_sp, CRS("+init=epsg:28356"))
RedDogPointsUTM <- spTransform(random_RedDogpoints_sp, CRS("+init=epsg:28356"))



#Data is cleaned- now we are going to estimate the 50% MCPs

#Lets get 50% MCP


#Calculate Kernel utilisation distributions----#Ignore warnings
#Calculate Kernel utilisation distributions----#Ignore warnings
Andy23KUD <- kernelUD(AndyPointsUTM, h = "href")
SWY23KUD <- kernelUD(SWYPointsUTM, h = "href")
RD23KUD <- kernelUD(RedDogPointsUTM, h = "href")
Bombah23KUD <- kernelUD(BombahPointsUTM, h ="href")
Tekka23KUD <- kernelUD(TekkaPointsUTM, h = "href")




#Calculate KDES####
Andy23CoreKDE <- getverticeshr(Andy23KUD, percent = 50)
SWY23CoreKDE <- getverticeshr(SWY23KUD, percent =50)
RD23CoreKDE <- getverticeshr(RD23KUD, percent = 50)
Bombah23CoreKDE <- getverticeshr(Bombah23KUD, percent = 50)
Tekka23CoreKDE <- getverticeshr(Tekka23KUD, percent =50)

#Calculate KDES
Andy23PeriphKDE <- getverticeshr(Andy23KUD, percent = 95)
SWY23PeriphKDE <- getverticeshr(SWY23KUD, percent =95)
RD23PeriphKDE <- getverticeshr(RD23KUD, percent = 95)
Bombah23PeriphKDE <- getverticeshr(Bombah23KUD, percent = 95)
Tekka23PeriphKDE <- getverticeshr(Tekka23KUD, percent =95)






AndyCore <- mcp(AndyPointsUTM, percent = 50)
SWYCore <- mcp(SWYPointsUTM, percent =50)
BombahCore <- mcp(BombahPointsUTM, percent = 50)
TekkaCore <- mcp(TekkaPointsUTM, percent = 50)
RedDogCore <- mcp(RedDogPointsUTM, percent =50)

AndyPeriph <- mcp(AndyPointsUTM, percent = 95)
SWYPeriph <- mcp(SWYPointsUTM, percent =95)
BombahPeriph <- mcp(BombahPointsUTM, percent = 95)
TekkaPeriph <- mcp(TekkaPointsUTM, percent = 95)
RedDogPeriph <- mcp(RedDogPointsUTM, percent =95)


#Give them names 


Andy23CoreKDE$CollarPack <- "JB"
SWY23CoreKDE$CollarPack <- "WT"
RD23CoreKDE$CollarPack <- "SL"
Bombah23CoreKDE$CollarPack <- "MB"
Tekka23CoreKDE$CollarPack <- "YG"

Andy23PeriphKDE$CollarPack <- "JB"
SWY23PeriphKDE$CollarPack <- "WT"
RD23PeriphKDE$CollarPack <- "SL"
Bombah23PeriphKDE$CollarPack <- "MB"
Tekka23PeriphKDE$CollarPack <- "YG"


AndyCore$CollarPack <- "JB"
SWYCore$CollarPack <- "WT"
RedDogCore$CollarPack <- "SL"
BombahCore$CollarPack <- "MB"
TekkaCore$CollarPack <- "YG"

AndyPeriph$CollarPack <- "JB"
SWYPeriph$CollarPack <- "WT"
RedDogPeriph$CollarPack <- "SL"
BombahPeriph$CollarPack <- "MB"
TekkaPeriph$CollarPack <- "YG"

#Give them names 

Andy23CoreKDE$TerritoryZone <- "Core"
SWY23CoreKDE$TerritoryZone <- "Core"
RD23CoreKDE$TerritoryZone <- "Core"
Bombah23CoreKDE$TerritoryZone <- "Core"
Tekka23CoreKDE$TerritoryZone <- "Core"

Andy23PeriphKDE$TerritoryZone <- "Periph"
SWY23PeriphKDE$TerritoryZone <- "Periph"
RD23PeriphKDE$TerritoryZone <- "Periph"
Bombah23PeriphKDE$TerritoryZone <- "Periph"
Tekka23PeriphKDE$TerritoryZone <- "Periph"


AndyCore$TerritoryZone <- "Core"
SWYCore$TerritoryZone <- "Core"
RedDogCore$TerritoryZone <- "Core"
BombahCore$TerritoryZone <- "Core"
TekkaCore$TerritoryZone <- "Core"

AndyPeriph$TerritoryZone <- "Periph"
SWYPeriph$TerritoryZone <- "Periph"
RedDogPeriph$TerritoryZone <- "Periph"
BombahPeriph$TerritoryZone <- "Periph"
TekkaPeriph$TerritoryZone <- "Periph"


#merge 

AllCore <- rbind(AndyCore, SWYCore, RedDogCore, BombahCore, TekkaCore)
AllAll <- rbind(AndyCore, SWYCore, RedDogCore, BombahCore, TekkaCore, AndyPeriph, SWYPeriph, RedDogPeriph, BombahPeriph, TekkaPeriph)

AllCoreKde <- rbind(Andy23CoreKDE,SWY23CoreKDE,RD23CoreKDE,Bombah23CoreKDE,Tekka23CoreKDE)

AllAllKde <- rbind(Andy23CoreKDE,SWY23CoreKDE,RD23CoreKDE,Bombah23CoreKDE,Tekka23CoreKDE,Andy23PeriphKDE,SWY23PeriphKDE,RD23PeriphKDE,Bombah23PeriphKDE,Tekka23PeriphKDE)


AllCoresf23 <- st_as_sf(AllCore, crs =32756)
AllCoresf23 <- st_transform(AllCoresf23, crs = 32756)
AllAllsf23 <- st_as_sf(AllAll, crs = 32756)
AllAllsf23 <- st_transform(AllAllsf23, crs = 32756)

AllAllCoreKdesf23 <- st_as_sf(AllCoreKde)
AllAllCoreKdesf23 <- st_transform(AllAllCoreKdesf23, crs = 32756)


AllAllKdesf23 <- st_as_sf(AllAllKde)
AllAllKdesf23 <- st_transform(AllAllKdesf23, crs = 32756)


plot(AllAllsf23)




#And for checking traps in each:

trapsall2023 <- read.csv(file = "Raw Data/UTMtraps.csv", header = TRUE)
trap_sf22 <- st_as_sf(trapsall2023, coords = c("x", "y"), crs = 32756) 
trap_sf23 <- st_as_sf(trapsall2023, coords = c("x", "y"), crs = 32756) 


#this just for plotting in next script. 
trap_sf_wgs84 <- st_transform(trap_sf22, crs = 4326)
trap_sf_wgs84$Longitude <- st_coordinates(trap_sf_wgs84)[,1]
trap_sf_wgs84$Latitude <- st_coordinates(trap_sf_wgs84)[,2]


#####Plot all the home ranges


coreandperiphareas22 <-ggplot() +
  # Plot home ranges
  geom_sf(data = AllAllsf22, aes(fill = CollarPack), alpha = 0.3, color = "black") +
  
  geom_sf(data = trap_sf22, size = 5, shape = 21) +  # Customize the shape and fill if needed
  # Labels and theme
  labs(title = "2022", 
       x = "Longitude", y = "Latitude", 
       color = "Status") +
  theme_minimal()+
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size =25),
        axis.text = element_text(size=20),
        axis.title = element_text(size=25),
        title= element_text(size=25))+
  theme(legend.position = "bottom")

coreandperiphareas23 <-ggplot() +
  # Plot home ranges
  geom_sf(data = AllAllsf23, aes(fill = CollarPack), alpha = 0.3, color = "black") +
  
  geom_sf(data = trap_sf22
          , size = 5, shape = 21) +
  
  # Labels and theme
  labs(title = "2023", 
       x = "Longitude", y = "Latitude", 
       color = "Status") +
  theme_minimal()+
  scale_x_continuous(n.breaks = 4)+
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size =25),
        axis.text = element_text(size=20),
        axis.title = element_text(size=25),
        title = element_text(size=25))+
  theme(legend.position = "bottom")

png("Figures/Supplementary/MCPs.png", width = 20, height = 13, res= 300, units = "in")

coreandperiphareas22+coreandperiphareas23

dev.off()


#lets see whats inside or not. 

#core and peripheral datasets are: 
#2022
#Core: AllCoresf22kde 
#Periph: #AllAllsf22kde

#Create columns
pack_names22 <- unique(AllAllsf22$CollarPack)

for (pack in pack_names22) {
  trap_sf22[[pack]] <- "OOR"  # Default value is "OOR"
}

for (i in 1:nrow(AllAllsf22)) {
  pack <- AllAllsf22$CollarPack[i]
  polygon <- AllAllsf22[i, ]  # Extract the polygon for the current pack
  
  # Check which traps are within the polygon
  within_polygon <- st_within(trap_sf22, polygon, sparse = FALSE)
  
  # Assign "COR" to traps within the polygon for the current pack
  trap_sf22[[pack]][within_polygon] <- "IHR"
}




#2023 
#Core: AllAllCoresf23
#Periph: AllAllsf23


#Create columns
pack_names23 <- unique(AllAllsf23$CollarPack)

for (pack in pack_names23) {
  trap_sf23[[pack]] <- "OOR"  # Default value is "OOR"
}

for (i in 1:nrow(AllAllsf23)) {
  pack <- AllAllsf23$CollarPack[i]
  polygon <- AllAllsf23[i, ]  # Extract the polygon for the current pack
  
  # Check which traps are within the polygon
  within_polygon <- st_within(trap_sf23, polygon, sparse = FALSE)
  
  # Assign "COR" to traps within the polygon for the current pack
  trap_sf23[[pack]][within_polygon] <- "IHR"
}


####finally for these two trap files, lets get them into a well structured df

#as some were slightly moved. 
trap_mapping <- c("PS24" = "PS4", "PS23" = "PS7", "PS25" = "PS10", "PS27" = "PS22")

trap_sf22 <- trap_sf22 %>%
  mutate(Trap = ifelse(Trap %in% names(trap_mapping), trap_mapping[Trap], Trap))

#Remove PS26, as it is only active for year 2
trap_sf22 <- trap_sf22[-which(trap_sf22$Trap == "PS26"),]

#repeat 23
trap_sf23 <- trap_sf23 %>%
  mutate(Trap = ifelse(Trap %in% names(trap_mapping), trap_mapping[Trap], Trap))

#Remove PS26, as it is only active for year 2
trap_sf23 <- trap_sf23[-which(trap_sf23$Trap == "PS26"),]





#get centroids and distances of traps to centroid
AllAllsf22$Centre <- st_centroid(AllAllsf22$geometry)
AllAllsf23$Centre <- st_centroid(AllAllsf23$geometry)

Allperiph22 <- AllAllsf22%>%
  filter(TerritoryZone=='Periph')

Allperiph23 <- AllAllsf23%>%
  filter(TerritoryZone=='Periph')

packnames22 <- Allperiph22$CollarPack
packnames23 <- Allperiph23$CollarPack


#get distance 22:
distances22 <- as.data.frame(st_distance(trap_sf22$geometry, Allperiph22$Centre))
distances23 <- as.data.frame(st_distance(trap_sf23$geometry, Allperiph23$Centre))


colnames(distances22) <- paste0("dist_to_",packnames22)
colnames(distances23) <- paste0("dist_to_",packnames23)

trap_sf22 <- bind_cols(trap_sf22, distances22)
trap_sf23 <- bind_cols(trap_sf23, distances23)

trap_sf22plotlater<- trap_sf22
trap_sf23plotlater<- trap_sf23

trap_sf22plotlater$Session <- 1
trap_sf23plotlater$Session <- 2

combinedtraps <- rbind(trap_sf22,trap_sf23)
combinedtrapsplotlater <- rbind(trap_sf22plotlater,trap_sf23plotlater)

