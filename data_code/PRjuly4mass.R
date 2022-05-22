library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)
library(wesanderson)

#reading in the file created by "airmasterk.R"
airmasterk <- read_feather("airmasterk.feather")

#filtering for the date range 
july4masterk <- airmasterk %>%
  filter(
    month(Time) == 7,
    (day(Time) == 3 & hour(Time) > 15) | day(Time) == 4 | (day(Time) == 5 & hour(Time) < 17),
    
  )

#July 4th Mass Melted for Legend
ggplot(gather(july4masterk,`Particle Diameter (microns)`, Mass, mass0.4:mass2.5),
       aes(Time,Mass, color = `Particle Diameter (microns)`)) +
  geom_point(size = .3, alpha = .4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  scale_y_continuous(limits = c(0,15)) +
  labs(
    x = "Jul 3                     Jul 4                                      Jul 5  ",
    y = "Mass - microns/cubic meter",
    title = paste(
      "July 4th 2020 ")
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  ) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1"),labels = c("0.4","0.6","0.85","1.5","2.5")) +
  guides(colour = guide_legend(title = "Particle Diameter (\U00B5m)",title.theme = element_text(size =9),override.aes = list(size=3, alpha=1)))
