#4.2 - plotting hypothesis 4
library(ggplot2)
library(ggeffects)
library(patchwork)
library(marginaleffects)
library(stringr)
#Use plot-predictions function from ggeffects:

both_effect <- ggpredict(hypfour, terms = c("PupsTot","Status"))

both_effect$Sex <- ifelse(grepl("F", both_effect$group), "F",
                          ifelse(grepl("M", both_effect$group), "M", NA))

testingit$PropInRange <- 1-(testingit$PropOutOfRange)

interaction <- ggplot(both_effect,aes(x=x,y=predicted, color = group)) + 
  geom_line()+
  geom_ribbon(aes(ymin=conf.low,ymax = conf.high,fill = group),alpha=0.2,show.legend = FALSE)+
  geom_point(data = testingit, aes(x = PupsTot, y = PropInRange, color = Status), size = 5, shape = 16) + # Add raw data points
  labs(title = "", 
       x = "Number of pups in pack", 
       y = "Proportion of\n detections inside home range",
       color = "Status of dingo")+
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size =19),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18))

interaction



png("Figures/Proportions and Pups.png", width = 13, height = 11, res= 300, units = "in")

interaction
dev.off()
