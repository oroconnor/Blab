library(openair)
library(tidyverse)
library(lubridate)
library(feather)
library(wesanderson)

#read in the tibble prepared by weatherdata.R script
fulldf <- read_feather("fulldf.feather")

# Below this is calculating the difference between the 9m temp sensor
# in Red Hook and the 2m temp sensor. I'm suspicious that the sun might
# shine on this set up in a way that doesn't really indicate when a
# temperature inversion is happening. To be explored...

fulldf <- fulldf %>%
  mutate(
    inversion = temp_9m - temp_2m,
    month = month(Time)
  )
# 
# 
ggplot(subset(fulldf,month(Time) == 9 & day(Time) >20),
       aes(Time,y)) +
  geom_point(aes(y = inversion), size= .4) +
 # scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  labs(
    y = "Size of Inversion at these two heights (deg. C)",
    x = NULL,
    title = paste(
      "Temperature Inversion - Red Hook"
    ),
    subtitle = paste(
      "Temp. (C) at 9m minus Temp. at 2m"
    )) +
theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  ) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1")) 
  



ggplot(data = fulldf) +
  geom_point(mapping = aes(x = inversion, y = pm2.5), 
             color = "green", size = .2, alpha = .5) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(-4,10)) +
  labs(
    title = paste(
      "PM2.5 Values in Relation to Inverion in Red Hook Temps"
    ))



ggplot(fulldf,aes(inversion, pm2.5)) +
  geom_hex(bins = 30) +
  geom_smooth() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(-5,12)) +
  labs(
    title = paste(
      "PM2.5 Values in Relation to Temp. Inversion"
    ))


ggplot(fulldf,aes(precip_local, pm2.5)) +
  geom_hex(bins = 50) +
  geom_smooth() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,2)) +
  labs(
    title = paste(
      "PM2.5 Values in Relation to Precipitation"
    ))

ggplot(fulldf,aes(avg_wind_sonic_mph, pm2.5)) +
  geom_hex(bins = 35) +
  geom_smooth() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,30)) +
  labs(
    title = paste(
      "PM2.5 Values in Relation to Wind Speed"
    ))
