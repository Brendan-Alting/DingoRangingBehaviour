#4.DONT NEED---- THIS IS the file where I checked distance from HR, not just in or out.. There's no difference, great. SO this analysis was not used.

library(dplyr)
library(lme4)
library(ggplot2)


##################NOTHING HERE DW BOUT IT########################

grouped <- nounknown%>%
  group_by(Individual, Trap, Session)%>%
  summarise(count = n(),
            Status = first(Status),
            Sex = first(Sex),
            PupsTot = first(PupsTot),
            Trap_Status = first(Trap_Status),
            Pack = first(Pack))


grouped <- grouped %>%
  left_join(combinedtrapsplotlater, by = c("Trap","Session"))

grouped <- grouped%>%
  mutate(distfromHR = case_when(
    Pack == "JB" ~ dist_to_JB,
    Pack == "WT" ~ dist_to_WT,
    Pack == "SL" ~ dist_to_SL,
    Pack == "MB" ~ dist_to_MB,
    Pack == "YG" ~ dist_to_YG,
    TRUE ~ NA
  ))


##Add interaction variable- just in case, probs don't need
grouped<- grouped %>%
  mutate(StatSex =interaction(Status, Sex, drop = TRUE))


#Assign factors
grouped$StatSex <- as.factor(grouped$StatSex)
grouped$Status <- as.factor(grouped$Status)
grouped$PupsTot <- as.numeric(grouped$PupsTot)
grouped$Pack <- as.factor(grouped$Pack)
grouped$Session <- factor(grouped$Session,levels =c("1","2"))
grouped$distfromHR <- as.numeric(grouped$distfromHR)

grouptest <- grouped%>%
  filter(!Individual =='JBU2101')

grouptest$Status <- ifelse(grepl("SubDominant", grouptest$Status), "Subdominant", "Dominant")

grouptest <- grouptest %>%
  group_by(Individual,Session)%>%
  filter(distfromHR == max(distfromHR))

grouptest$distfromHR <- round(grouptest$distfromHR)

hypfournew <- glmer(distfromHR ~ PupsTot* Status + (1 | Pack), 
                 data = grouptest,
                 family = poisson,
                 control = glmerControl(optimizer = "bobyqa"))

summary(hypfournew)
hypfournew
