#Hypothesis 4. 
library(tidyverse)
library(lme4)
#First need to do some more processing: 


Lookingatpackandpup <- nounknown %>%
  group_by(Individual, Session)%>%
  summarize(CountIHR = sum(Trap_Status == "IHR"),
            CountOOR = sum(Trap_Status == "OOR"),
            CountTotal = CountIHR+CountOOR,
            PropOutOfRange = CountOOR/CountTotal,
            Status = Status,
            Sex = Sex,
            PupsTot = PupsTot,
            PackTot = PackTot,
            bred=bred,
            Pack=Pack)%>%
  distinct()


##Add interaction variable- just in case, probs don't need
Lookingatpackandpup<- Lookingatpackandpup %>%
  mutate(StatSex =interaction(Status, Sex, drop = TRUE))


#Assign factors
Lookingatpackandpup$Status <- as.factor(Lookingatpackandpup$Status)
Lookingatpackandpup$PupsTot <- as.numeric(Lookingatpackandpup$PupsTot)
Lookingatpackandpup$PackTot <- as.numeric(Lookingatpackandpup$PackTot)
Lookingatpackandpup$Pack <- as.factor(Lookingatpackandpup$Pack)
Lookingatpackandpup$Session <- factor(Lookingatpackandpup$Session,levels =c("1","2"))

#Rename 
Lookingatpackandpup <- Lookingatpackandpup %>%
  rename('IHR' = CountIHR,
         'Out_Of_Range' = CountOOR)

testingit <- Lookingatpackandpup%>%
  filter(!Individual =='JBU2101')

testingit$Status <- ifelse(grepl("SubDominant", testingit$Status), "Subdominant", "Dominant")

#IHR ==== In home range

###

hypfour <- glmer(cbind(IHR,Out_Of_Range) ~ PupsTot* Status + (1 | Pack) + (1 | Session), 
                    data = testingit, 
                    family = binomial,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(hypfour)
