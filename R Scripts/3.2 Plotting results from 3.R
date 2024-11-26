#3.1 Plotting hypothesis 3
library(ggplot2)
library(tidygam)
library(tidyverse)
library(dplyr)
#Use predict gam function within ggplot to  to plot this, they work together. 
gam_2 <- readRDS(file = "Derived Data/gam_2.rds")


plotfullgam <- predict_gam(gam_2,
                           exclude_terms = c("s(Pack)","s(Session)","s(week)"),
                           series = "week",
                           length_out = 100, 
                           tran_fun = exp)



plotrawdatadom <- almostthere %>%
  mutate(
  Sex = case_when(
    grepl("F", SexZone) ~ "Dominant lone\nfemale",
    grepl("M", SexZone) ~ "Dominant lone\nmale",
    grepl("Pair",SexZone)~ "Dominant female-male\npair"
  ),
  Zone = case_when(
    grepl("IHR", SexZone) ~ "Inside home range",
    grepl("OOR", SexZone) ~ "Outside home range"
  )) %>%
  select(week, Count, Sex, Zone)%>%
  filter(!Count==0)



plotdominantresults <- plotfullgam %>%
  mutate(Sex = case_when(
    grepl("F", SexZone) ~ "Dominant lone\nfemale",
    grepl("M", SexZone) ~ "Dominant lone\nmale",
    grepl("Pair",SexZone)~"Dominant female-male\npair"
  ),
  Zone = case_when(
    grepl("IHR", SexZone) ~ "Inside home range",
    grepl("OOR", SexZone) ~ "Outside home range"
  ))%>%
  ggplot(aes(x = week, y = Count)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.4) +
  facet_grid(Sex~ Zone) +
  theme_minimal() +
  theme(panel.spacing = unit(1,"lines"),
        panel.border = element_rect(0.1),
        panel.grid = element_blank(),
        text=element_text(size = 40),
        
        axis.text.x = element_text(size =20),
        legend.position = "none") +
  scale_x_continuous(breaks = c(0,4.2,8.4,12.6,16.8,21,25.2),
                     labels = c("Dec","Jan","Feb","Mar","Apr","May","Jun")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10),
                     labels = c("0","2","4","6","8","10"))+
  labs(x = "Month",
       y = "Detections",
       title = NULL)+
  scale_color_manual(values = c("grey", "grey", "grey"))+
  scale_fill_manual(values = c("grey", "grey", "grey"))+
  geom_point(data = plotrawdatadom, aes(x = week, y = Count, size = ifelse(Count == 0, 2, 5)), shape = 16, alpha = 0.2, position = position_jitter(width = 0, height = 0.1)) +
  scale_size_identity() +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", size = 1)+
  geom_vline(xintercept = 22, linetype = "dashed", color = "black", size = 1)+
  coord_cartesian(xlim = c(1,25.2))

plotdominantresults


png("Figures/Dominants Lone Visits.png", width = 19, height = 19, res= 300, units = "in")

plotdominantresults
dev.off()

