# Kingston Home Egg Explorer - 2020

library(shiny)
#library(feather)
library(ggplot2)
library(lubridate)
library(tidyverse)
#library(openair)
library(shinythemes)
library(gghighlight)

# Prepping the data file --------------------------------------------

webmasterk<- read_csv("big_egg2.csv") 

# %>%
#   select( #selects certain variables from dataset
#     timestamp_local, pm25, pm10
#   ) %>%

webmasterk <- webmasterk %>%
  rename( # Renames them so that they display nicely in plots
    YMD = time
  )

#Time Series plot
ggplot(subset(webmasterk, month(YMD) == 4 & day(YMD) > 5 & day(YMD) < 14), aes(YMD,y)) +
  geom_point(aes(y = pm2p5.house1out), size= .3, color = "red") +
  #scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  theme_classic() +
  gghighlight(
    #pm2p5.house1out > 20
    outdoorburn.house2 > 0
  )
  labs(
    y = "pm2.5",
    x = NULL,
    title = "",
    subtitle = paste(
      ""
    )) +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  )
#scale_y_continuous(limits = c(0,50))

