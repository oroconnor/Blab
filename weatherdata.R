library(tidyverse)
library(lubridate)
library(ggplot2)
library(openair)

weatherdata <- "Mesodata_master.csv"

weatherdf <- read_csv(weatherdata)

#change time strings to datetime type
weatherdf<-mutate(weatherdf, Time= parse_date_time(timestamp,"YmdHMS"))

weather <- weatherdf %>%
  select(Time,temp_2m,temp_9m,wind_dir_sonic,avg_wind_sonic_mph)


newdata <- "Copy of pcmaster_ktown_9-22.csv"

df <- read_csv(newdata)

#change time strings to datetime type
df<-mutate(df, Time= parse_date_time(Time,"mdYHM"))

#using average diameters for each bin to get count totals by size
airmasterk <- df %>%
  mutate(
    tot0.4count = c0.3 - c0.5 - c0.7 -c1.0 - c2.0 - c3.0 - c5.0 - c10.0,
    tot0.6count = c0.5 - c0.7 -c1.0 - c2.0 - c3.0 - c5.0 - c10.0,
    tot0.85count = c0.7 -c1.0 - c2.0 - c3.0 - c5.0 - c10.0,
    tot1.5count = c1.0 - c2.0 - c3.0 - c5.0 - c10.0,
    tot2.5count = c2.0 - c3.0 - c5.0 - c10.0,
    tot4.0count = c3.0 - c5.0 - c10.0,
    tot7.5count = c5.0 - c10.0,
    tot20count = c10.0,
  )


#using 1.65 g/cm3 as density as per paper, calculating mass for each count bin, resulting in ug/m3

airmasterk <- airmasterk %>%
  mutate(
    m0.4 = (0.0000000553*tot0.4count)/0.001,
    m0.6 = (0.000000187*tot0.6count)/0.001,
    m0.85 = (0.00000053*tot0.85count)/0.001,
    m1.5 = (0.00000291*tot1.5count)/0.001,
    m2.5 = (0.0000135*tot2.5count)/0.001,
    m4.0 = (0.0000553*tot4.0count)/0.001,
    m7.5 = (0.000364*tot7.5count)/0.001,
    m20 = (0.006908*tot20count)/0.001,
  )

airmasterk <- airmasterk %>% rowwise() %>%
  mutate(
    pm2.5 = sum(c(m0.4,m0.6,m0.85,m1.5,m2.5)),
    pm10 = sum(c(m0.4,m0.6,m0.85,m1.5,m2.5,m4.0,m7.5,m20)) 
  )

fulldf <- right_join(airmasterk, weather, by = "Time",copy = FALSE, suffix = c(".x", ".y"))

fulldf <- fulldf %>%
  mutate(
    ws = avg_wind_sonic_mph,
    wd = wind_dir_sonic
  )

fulldf <- fulldf %>%
  filter(
    pm2.5 >= 0,
    pm2.5 < 100
  )

july4df <- fulldf %>%
  filter(
    month(Time) == 7,
    day(Time) == 4
  )

#windRose(fulldf)
pollutionRose(july4df, pollutant = "pm2.5")
pollutionRose(fulldf, pollutant = "pm2.5")


fulldf <- fulldf %>%
  mutate(
    inversion = temp_9m - temp_2m,
    month = month(Time)
  )
# 
# 
# ggplot(subset(fulldf, month == 2),
#        aes(Time,y)) +
#   geom_point(aes(y = inversion), size= .4) +
#   labs(
#     y = "Inversion",
#     x = "Date and Hour",
#     title = paste(
#       "Inversion Potential - Red Hook"
#     ),
#     subtitle = paste(
#       "temp at 9m minus temp at 2m"
#     ))

ggplot(data = fulldf) +
  geom_point(mapping = aes(x = inversion, y = pm2.5), 
             color = "green", size = .2, alpha = .5) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(-4,10)) +
  labs(
    title = paste(
      "PM2.5 Values in Relation to Inverion in Red Hook Temps"
    ))

