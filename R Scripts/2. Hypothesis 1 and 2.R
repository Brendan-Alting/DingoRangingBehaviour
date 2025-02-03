#2. Hypothesis 1 and 2 testing
#Formatting for hypothesis 1 and 2, and modelling. 

library(dplyr)
library(tidyverse)
library(mgcv)
library(tidygam)
library(broom)
library(report)

dingomerged <- read.csv(file = "Derived Data/dingodata.csv", header =T)

#First we're going to sort it by month; 
nounknown <- dingomerged[-which(is.na(dingomerged$Pack)), ]
nounknown <- nounknown%>%
  mutate(bred = case_when(PupsTot>0~"Yes",
                          PupsTot==0~"No"))

#used in analysis
summarytotal <- nounknown%>%
  group_by(Individual, Session, Status)%>%
  summarise(Count = n())

summary_stats <- summarytotal %>%
  group_by(Session, Status) %>%
  summarise(
    mean_count = mean(Count),               # Mean of Count
    range_count = paste(min(Count), max(Count), sep = " to ")  # Range of Count (min to max)
  )

Lookingatpackandpupweek <- nounknown %>%
  group_by(Individual, Session, week)%>%
  summarize(CountIHR = sum(Trap_Status == "IHR"),
            CountOOR = sum(Trap_Status == "OOR"),
            Status = Status,
            Sex = Sex,
            Pack = Pack,
            PupsTot = PupsTot,
            PackTot = PackTot,
            bred=bred)%>%
  distinct()

Lookingatpackandpupweek <- Lookingatpackandpupweek %>%
  pivot_longer(cols = starts_with("Count"), names_to = "Zone", values_to = "Count") %>%
  mutate(
    Zone = recode(Zone, "CountIHR" = "IHR", "CountOOR" = "OOR")
  ) %>%
  ungroup() %>%  
  complete(week, Status, Sex, Session, Pack, Zone, fill = list(Count = 0)) %>%
  group_by(week, Status, Sex, Session, Pack, Zone) %>%
  summarize(Count = sum(Count, na.rm = TRUE), .groups = "drop")


#We are here extending dataset so that each month and each zone has a value. It's count data, so zeroes need to be present. 


####Now adding more variables we need. 


Lookingatpackandpupweek<- Lookingatpackandpupweek%>%
  mutate(StatSexZone = interaction(Status,Sex,Zone, drop = TRUE))
Lookingatpackandpupweek$Status <- as.factor(Lookingatpackandpupweek$Status)


#####Define weeks in numeric
Lookingatpackandpupweek$week <- as.numeric(Lookingatpackandpupweek$week)

#Asssign factors to all variables

Lookingatpackandpupweek$Status <- factor(Lookingatpackandpupweek$Status, levels = c("Dominant", "SubDominant"))

Lookingatpackandpupweek$Sex <- factor(Lookingatpackandpupweek$Sex, levels = c("F", "M"))

Lookingatpackandpupweek$Zone <- factor(Lookingatpackandpupweek$Zone, levels = c("IHR","OOR"))

Lookingatpackandpupweek$Pack <- factor(Lookingatpackandpupweek$Pack, levels = c("JB","WT","SL","MB",'YG'))

Lookingatpackandpupweek$Session <- factor(Lookingatpackandpupweek$Session, levels = c('1','2'))



Lookingatpackandpupweek$StatSexZone <- factor(interaction(Lookingatpackandpupweek$Status,Lookingatpackandpupweek$Sex,Lookingatpackandpupweek$Zone))

Lookingatpackandpupweek$StatSex <- factor(interaction(Lookingatpackandpupweek$Status,Lookingatpackandpupweek$Sex))


#We've now got data in correct format, we can run our models



#model
gam_model <- gam(Count ~ 
                   s(week, bs = "cr") + 
                   s(week, by = StatSexZone, bs = "cr") +
                   StatSexZone +
                   s(Pack, bs = "re") +
                   s(Session, bs = "re"),
                 family = nb(),
                 method = "REML", # Penalized likelihood method
                 data = Lookingatpackandpupweek,
                 control = gam.control(maxit = 1000))

summary(gam_model)
saveRDS(gam_model, file = "Derived Data/gam_1.rds")

gam1_summary <- tidy(gam_model, parametric = TRUE, conf.int = TRUE) 
gam1_smooth <- tidy(gam_model, parametric = FALSE, conf.int = TRUE)

write.csv(gam1_summary, "Derived Data/gam1para.csv")
write.csv(gam1_smooth, "Derived Data/gam1smooth.csv")

