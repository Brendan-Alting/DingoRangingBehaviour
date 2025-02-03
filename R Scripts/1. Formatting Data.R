###1 Arrange data to get datasets suitable for models. 
library(dplyr)
library(tidyverse)
library(lubridate)
#First we will read in the dataframe which contains deomgraphic information from each individual, from each year. 

dingostatuses22 <- read.csv(file = "Raw Data/DingoDems.csv", header = T)
dingostatuses23 <- read.csv(file = "Raw Data/DingoDems.csv", header = T)

#using trap_sf files from 0. collar data formatting
trap_long_22 <- pivot_longer(trap_sf22, cols = JB:YG, names_to = "Pack", values_to = "Trap_Status")

trap_long_23 <- pivot_longer(trap_sf23, cols = JB:YG, names_to = "Pack", values_to = "Trap_Status")

dingostatuses22 <- merge(dingostatuses22, trap_long_22, by = "Pack")
dingostatuses23 <- merge(dingostatuses23,trap_long_23,by = "Pack")

dingostatusesboth <- dingostatuses22

dingostatusesboth$Trap_Status <- ifelse(dingostatusesboth$Session == 1,
                                             dingostatuses22$Trap_Status,
                                             dingostatuses23$Trap_Status)

dingostatusesboth <- dingostatusesboth%>%
  group_by(Individual,Session)%>%
  mutate(TrapsInRange = sum(Trap_Status=='IHR'))

#Also read in detections dataset, which contains all dingo detections from both years

dingodetections <- read.csv(file = "Raw Data/DingoDetections.csv", header = T)

#now need to merge some traps, as they were the same location, just moved slightly
trap_mapping <- c("PS24" = "PS4", "PS23" = "PS7", "PS25" = "PS10", "PS27" = "PS22")


dingodetections <- dingodetections %>%
  mutate(Trap = ifelse(Trap %in% names(trap_mapping), trap_mapping[Trap], Trap))

#Remove PS26, as it is only active for year 2
dingodetections <- dingodetections[-which(dingodetections$Trap == "PS26"),]

#and change an incorrect records
dingodetections <- dingodetections%>%
  filter(!(Individual=="UOM1707" & Trap == "PS5"))


#Now we are going to combine the two dataframes, taking the right information from each. 

dingomerged <- left_join(dingodetections, dingostatusesboth%>% dplyr::select(Status, Pack, Sex, Individual, Session,PupsTot,PackTot, Trap_Status, Trap,TrapsInRange), by = c("Individual" = "Individual", "Session" = "Session", "Trap" = "Trap"))




#now remove unidentified individuals:
dingomerged <- dingomerged[-which(dingomerged$Individual == "Unidentifiable"),]


#Now we are going to subset down to correct dates

#Using just 7day survey periods- so 1 week. Do summed detections on cameras. 
dingomerged$Date <- as.Date(dingomerged$Date)
#First get dates in month and day (Same time period, different year).  

dingomerged <- dingomerged %>%
  mutate(DateNoYear = format(Date, "%m-%d"))

#Filter to correct dates
dingomerged <- dingomerged %>%
  filter((Date >= as.Date("2021-12-05") & Date <= as.Date("2022-05-28")) |
           (Date >= as.Date("2022-12-05") & Date <= as.Date("2023-05-28")))

#remove another false individual based on date. 
dingomerged <- dingomerged%>%
  filter(!(Individual == "UOM2007" & Date == "2023-03-05
"))


#Get dates and assign a categorical code-this are equal number of days, so the counts are as a ratio. 
dingomerged <- dingomerged %>%
  mutate(month = case_when(
    between(Date, as.Date("2021-12-05"), as.Date("2022-01-02")) |
      between(Date, as.Date("2022-12-05"), as.Date("2023-01-02")) ~ "Dec",
    between(Date, as.Date("2022-01-03"), as.Date("2022-01-31")) |
      between(Date, as.Date("2023-01-03"), as.Date("2023-01-31")) ~ "Jan",
    between(Date, as.Date("2022-02-01"), as.Date("2022-03-01")) |
      between(Date, as.Date("2023-02-01"), as.Date("2023-03-01")) ~ "Feb",
    between(Date, as.Date("2022-03-02"), as.Date("2022-03-30")) |
      between(Date, as.Date("2023-03-02"), as.Date("2023-03-30")) ~ "Mar",
    between(Date, as.Date("2022-03-31"), as.Date("2022-04-28")) |
      between(Date, as.Date("2023-03-31"), as.Date("2023-04-28")) ~ "Apr",
    between(Date, as.Date("2022-04-29"), as.Date("2022-05-28")) |
      between(Date, as.Date("2023-04-29"), as.Date("2023-05-28")) ~ "May",
    TRUE ~ NA_character_  # For dates that don't fall in the defined ranges
  ))

###Also weeks: 

dingomerged <- dingomerged %>%
  mutate(week = case_when(
    between(Date, as.Date("2021-12-05"), as.Date("2021-12-11")) ~ "1",
    between(Date, as.Date("2021-12-12"), as.Date("2021-12-18")) ~ "2",
    between(Date, as.Date("2021-12-19"), as.Date("2021-12-25")) ~ "3",
    between(Date, as.Date("2021-12-26"), as.Date("2022-01-01")) ~ "4",
    between(Date, as.Date("2022-01-02"), as.Date("2022-01-08")) ~ "5",
    between(Date, as.Date("2022-01-09"), as.Date("2022-01-15")) ~ "6",
    between(Date, as.Date("2022-01-16"), as.Date("2022-01-22")) ~ "7",
    between(Date, as.Date("2022-01-23"), as.Date("2022-01-29")) ~ "8",
    between(Date, as.Date("2022-01-30"), as.Date("2022-02-05")) ~ "9",
    between(Date, as.Date("2022-02-06"), as.Date("2022-02-12")) ~ "10",
    between(Date, as.Date("2022-02-13"), as.Date("2022-02-19")) ~ "11",
    between(Date, as.Date("2022-02-20"), as.Date("2022-02-26")) ~ "12",
    between(Date, as.Date("2022-02-27"), as.Date("2022-03-05")) ~ "13",
    between(Date, as.Date("2022-03-06"), as.Date("2022-03-12")) ~ "14",
    between(Date, as.Date("2022-03-13"), as.Date("2022-03-19")) ~ "15",
    between(Date, as.Date("2022-03-20"), as.Date("2022-03-26")) ~ "16",
    between(Date, as.Date("2022-03-27"), as.Date("2022-04-02")) ~ "17",
    between(Date, as.Date("2022-04-03"), as.Date("2022-04-09")) ~ "18",
    between(Date, as.Date("2022-04-10"), as.Date("2022-04-16")) ~ "19",
    between(Date, as.Date("2022-04-17"), as.Date("2022-04-23")) ~ "20",
    between(Date, as.Date("2022-04-24"), as.Date("2022-04-30")) ~ "21",
    between(Date, as.Date("2022-05-01"), as.Date("2022-05-07")) ~ "22",
    between(Date, as.Date("2022-05-08"), as.Date("2022-05-14")) ~ "23",
    between(Date, as.Date("2022-05-15"), as.Date("2022-05-21")) ~ "24",
    between(Date, as.Date("2022-05-22"), as.Date("2022-05-28")) ~ "25",
    between(Date, as.Date("2022-05-29"), as.Date("2022-06-04")) ~ "26",
    between(Date, as.Date("2022-06-05"), as.Date("2022-06-11")) ~ "27",
    between(Date, as.Date("2022-06-12"), as.Date("2022-06-18")) ~ "28",
    between(Date, as.Date("2022-06-19"), as.Date("2022-06-25")) ~ "29",

    # Repeat weeks for the year 2023
    between(Date, as.Date("2022-12-05"), as.Date("2022-12-11")) ~ "1",
    between(Date, as.Date("2022-12-12"), as.Date("2022-12-18")) ~ "2",
    between(Date, as.Date("2022-12-19"), as.Date("2022-12-25")) ~ "3",
    between(Date, as.Date("2022-12-26"), as.Date("2023-01-01")) ~ "4",
    between(Date, as.Date("2023-01-02"), as.Date("2023-01-08")) ~ "5",
    between(Date, as.Date("2023-01-09"), as.Date("2023-01-15")) ~ "6",
    between(Date, as.Date("2023-01-16"), as.Date("2023-01-22")) ~ "7",
    between(Date, as.Date("2023-01-23"), as.Date("2023-01-29")) ~ "8",
    between(Date, as.Date("2023-01-30"), as.Date("2023-02-05")) ~ "9",
    between(Date, as.Date("2023-02-06"), as.Date("2023-02-12")) ~ "10",
    between(Date, as.Date("2023-02-13"), as.Date("2023-02-19")) ~ "11",
    between(Date, as.Date("2023-02-20"), as.Date("2023-02-26")) ~ "12",
    between(Date, as.Date("2023-02-27"), as.Date("2023-03-05")) ~ "13",
    between(Date, as.Date("2023-03-06"), as.Date("2023-03-12")) ~ "14",
    between(Date, as.Date("2023-03-13"), as.Date("2023-03-19")) ~ "15",
    between(Date, as.Date("2023-03-20"), as.Date("2023-03-26")) ~ "16",
    between(Date, as.Date("2023-03-27"), as.Date("2023-04-02")) ~ "17",
    between(Date, as.Date("2023-04-03"), as.Date("2023-04-09")) ~ "18",
    between(Date, as.Date("2023-04-10"), as.Date("2023-04-16")) ~ "19",
    between(Date, as.Date("2023-04-17"), as.Date("2023-04-23")) ~ "20",
    between(Date, as.Date("2023-04-24"), as.Date("2023-04-30")) ~ "21",
    between(Date, as.Date("2023-05-01"), as.Date("2023-05-07")) ~ "22",
    between(Date, as.Date("2023-05-08"), as.Date("2023-05-14")) ~ "23",
    between(Date, as.Date("2023-05-15"), as.Date("2023-05-21")) ~ "24",
    between(Date, as.Date("2023-05-22"), as.Date("2023-05-28")) ~ "25",
    between(Date, as.Date("2023-05-29"), as.Date("2023-06-04")) ~ "26",
    between(Date, as.Date("2023-06-05"), as.Date("2023-06-11")) ~ "27",
    between(Date, as.Date("2023-06-12"), as.Date("2023-06-18")) ~ "28",
    between(Date, as.Date("2023-06-19"), as.Date("2023-06-25")) ~ "29",
    TRUE ~ NA_character_
  ))












###lets add HR data into df. 




#summary merge: 

summary_merge <- dingomerged %>%
  group_by(Individual,Trap,Pack, Session,week,Status,Sex,PupsTot,TrapsInRange,Trap_Status) %>%
  summarise(Count = n(), .groups = 'drop')

summary_wide <-  summary_merge %>%
  pivot_wider(names_from = Trap, values_from = Count, values_fill = list(Count = 0))





summary_wide <- summary_wide %>%
  rowwise() %>%
  mutate(trapsvisited = sum(c_across(PS3:PS9) != 0),
         trapsbyavail = trapsvisited/TrapsInRange) %>%
  ungroup()%>%
  filter(!PupsTot=="N/A")#remove ones for which we didn't have enough demographic data. 


#summary statistics for reviewer. How many detections of each individual? 
summarytotal <- dingomerged%>%
  group_by(Individual)%>%
  summarise(Count = n())

dingodata <- (as.data.frame(dingomerged))
write.csv(dingodata, "Derived Data/dingodata.csv", row.names = T)

#end. 

