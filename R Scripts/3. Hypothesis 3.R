#3. Hypothesis 3
library(dplyr)
library(mgcv)
library(broom)

library(lubridate)
######################below is a lot of manipulating, to get the data down to either lone or together individuals.

dingomerged <- read.csv(file = "Derived Data/dingodata.csv", header =T)

dominants <- dingomerged[which(dingomerged$Status == "Dominant"),]
dominants$hour <- hour(dominants$DateTime)

dominants <- dominants[-which(dominants$hour < 5),]
dominants <- dominants[-which(dominants$hour > 21),]
#First datetime within 5 mins
dominants$infive <- FALSE

for (i in 1:nrow(dominants)) {
  for (j in 1:nrow(dominants)) {
    # Check if the DateTime values of rows i and j are within 5 minutes of each other
    if (i != j && abs(difftime(dominants$DateTime[i], dominants$DateTime[j], units = "mins")) <= 5) {
      # If they are, set 'infive' to TRUE using logical OR
      dominants$infive[i] <- dominants$infive[i] | TRUE
      dominants$infive[j] <- dominants$infive[j] | TRUE
    }
  }
}

dominants <- dominants%>%
  group_by(Trap)%>%
  mutate(Who = ifelse(infive, "Pair", "Lone"))

##DO RUN need to change MBF1701 row 1, and UOM1707 to LONE-------This is to correct a few errors in the above, a bit messy - do not do like this if you read this, fix ur data first 
dominants$Who[1] <- "Lone" #MBF1701  "2021-12-07 15:36:42"
dominants$Who[227] <- "Lone" #UOM1707  "2022-05-03 15:19:58"
dominants$Who[359] <- "Lone" #UOF1801  "2023-02-07 06:40:56"
dominants$Who[234] <- "Lone" #UOM1707  "2022-12-21 06:39:33"
dominants$Who[466] <- "Lone" #UOM2002  "2022-12-21 06:42:47"




almostthere <- dominants %>%
  group_by(Trap_Status, week, Session, Pack) %>%
  summarize(
    CountLON_M = sum(Who == "Lone" & Sex == "M"),
    CountLON_F = sum(Who == "Lone" & Sex == "F"),
    CountPAIR = sum(Who == "Pair"),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(CountLON_M, CountLON_F, CountPAIR), 
               names_to = "Type", values_to = "Count") %>%
  mutate(
    Sex = case_when(
      Type == "CountLON_M" ~ "M",
      Type == "CountLON_F" ~ "F",
      Type == "CountPAIR" ~ "Pair"
    ),
    Zone = case_when(
      Type == "CountLON_M" ~ "LON",
      Type == "CountLON_F" ~ "LON",
      Type == "CountPAIR" ~ "PAIR"
    )
  ) 

almostthere <- almostthere%>%
  complete(week, Trap_Status, Sex, Session, Pack, , fill = list(Count = 0)) %>%
  group_by(week, Trap_Status, Sex, Session, Pack) %>%
  summarize(Count = sum(Count, na.rm = TRUE), .groups = "drop")


#now we're assigning traps again. 

almostthere$Trap_Status <- factor(almostthere$Trap_Status, levels = c("IHR", "OOR"))
almostthere$Sex <- factor(almostthere$Sex, levels = c("M", "F","Pair"))

almostthere$Session <- factor(almostthere$Session, levels = c("1","2"))
almostthere$Pack <- factor(almostthere$Pack, levels = c("JB","WT","SL","MB",'YG'))
almostthere$SexZone <- factor(interaction(almostthere$Sex,almostthere$Trap_Status))
almostthere$week <- as.numeric(almostthere$week)



gam_2 <-gam(Count ~ 
              s(week) +
              s(week, by = SexZone) +
              SexZone +
              s(Pack, bs = "re") +
              s(Session, bs = "re"),
            family = nb(),
            method = "REML",
            data = almostthere,
            control = gam.control(maxit = 1000))

            
summary(gam_2)

saveRDS(gam_2, file = "Derived Data/gam_2.rds")


gam2_summary <- tidy(gam_2, parametric = TRUE, conf.int = TRUE) 
gam2_smooth <- tidy(gam_2, parametric = FALSE, conf.int = TRUE)

write.csv(gam2_summary, "Derived Data/gam2para.csv")
write.csv(gam2_smooth, "Derived Data/gam2smooth.csv")
