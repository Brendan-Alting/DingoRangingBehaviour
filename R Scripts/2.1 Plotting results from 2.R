##2.1 plotting results from hypothesis 2. 
library(ggplot2)
library(tidygam)
library(tidyverse)

gam_1 <- readRDS(file = "Derived Data/gam_1.rds")


####Maybe delete above (maybe delete below)

plotrawdata <- Lookingatpackandpupweek %>%
  mutate(Status = case_when(
    str_detect(StatSexZone, "^Dominant") ~ "Dominant",
    str_detect(StatSexZone, "^SubDominant") ~ "Subdominant"
  ),
  Sex = case_when(
    grepl("F", StatSexZone) ~ "Female",
    grepl("M", StatSexZone) ~ "Male"
  ),
  Zone = case_when(
    grepl("IHR", StatSexZone) ~ "Inside home range",
    grepl("OOR", StatSexZone) ~ "Outside home range"
  )) %>%
  select(week, Count, Status, Sex, Zone)%>%
  filter(!Count==0)

plotfullgam <- predict_gam(gam_model,
                           exclude_terms = c("s(Pack)","s(Session)","s(week)"),
                           series = "week",
                           length_out = 100, 
                           tran_fun = exp)




plotfullgam <- plotfullgam %>%
  mutate(
    Status = case_when(
      str_detect(StatSexZone, "^Dominant") ~ "Dominant",
      str_detect(StatSexZone, "^SubDominant") ~ "Subdominant"
    ),
    Sex = case_when(
      grepl("F", StatSexZone) ~ "Female",
      grepl("M", StatSexZone) ~ "Male"
    ),
    Zone = case_when(
      grepl("IHR", StatSexZone) ~ "Inside home range",
      grepl("OOR", StatSexZone) ~ "Outside home range"
    )
  )


plotfullresults <- plotfullgam %>%
  ggplot(aes(x = week, y = Count, color = Status)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Status), alpha = 0.3) +
  facet_grid(Sex ~ Zone) +
  scale_size_identity() +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(0.1),
    panel.grid = element_blank(),
    text = element_text(size = 40),
    axis.text.x = element_text(size = 20)
  ) +
  scale_x_continuous(
    breaks = c(0, 4.2, 8.4, 12.6, 16.8, 21, 25.2),
    labels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")
  ) +
  scale_y_continuous(
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  labs(x = "Month", y = "Detections", title = "") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 22, linetype = "dashed", color = "black", size = 1) +
  coord_cartesian(xlim = c(1, 25.2))

plotfullresults



png("Figures/Dingo Territorial Detections.png", width = 18, height = 13, res= 300, units = "in")

plotfullresults

dev.off()


#Raw data included for supp materials




plotraw <- plotfullgam %>%
  ggplot(aes(x = week, y = Count, color = Status)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Status), alpha = 0.3) +
  facet_grid(Sex ~ Zone) +
  # Overlay raw data
  geom_point(data = plotrawdata, aes(x = week, y = Count, color = Status, size = ifelse(Count == 0, 2, 4)), 
             shape = 16, alpha = 0.3, position = position_jitter(width = 0, height = 0.1)) +
  scale_size_identity() +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(0.1),
    panel.grid = element_blank(),
    text = element_text(size = 40),
    axis.text.x = element_text(size = 20)
  ) +
  scale_x_continuous(
    breaks = c(0, 4.2, 8.4, 12.6, 16.8, 21, 25.2),
    labels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")
  ) +
  scale_y_continuous(
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c("0", "2", "4", "6", "8", "10")
  ) +
  labs(x = "Month", y = "Detections", title = "") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 22, linetype = "dashed", color = "black", size = 1) +
  coord_cartesian(xlim = c(1, 25.2), ylim = c(0,10))

plotraw



png("Figures/Supplementary/Fig raw.png", width = 18, height = 13, res= 300, units = "in")

plotraw
dev.off()

