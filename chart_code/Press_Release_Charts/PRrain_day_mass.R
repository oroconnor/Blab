###creates some plots looking at particle mass for rainy spring day


library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)


#reading in the file created by "airmasterk.R"
airmasterk <- read_feather("airmasterk.feather")

#filtering for the date range 
rainmarchk <-airmasterk %>%
  filter(
    month(Time) == 3,
    (day(Time) == 12 & hour(Time) > 11) | day(Time) == 13 | (day(Time) == 14 & hour(Time) < 13),
    
  )
#Rainy March Day Mass Melted for Legend
ggplot(subset(gather(rainmarchk,`Particle Diameter (microns)`, Mass, mass0.4:mass2.5)),
       aes(Time,Mass, color = `Particle Diameter (microns)`)) +
  geom_point(size = .3, alpha = .4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  scale_y_continuous(limits = c(0,10)) +
  labs(
    x = "Mar 12                            Mar 13                              Mar 14",
    y = "Mass - microns/cubic meter",
    subtitle = paste(
      "Rain Highlighted in Blue ")
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  ) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1"),labels = c("0.4","0.6","0.85","1.5","2.5")) +
  guides(colour = guide_legend(title = "Particle Diameter (\U00B5m)",title.theme = element_text(size =9),override.aes = list(size=3, alpha=1))) +
  annotate("rect", xmin = ymd_hms("2020-03-12 23:30:00"), xmax = ymd_hms("2020-03-13 11:30:00"), ymin = 0, ymax = 10,
           alpha = .1, fill = "blue")
