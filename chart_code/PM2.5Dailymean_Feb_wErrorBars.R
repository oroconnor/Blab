library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

#read in the tibble prepared by airmasterk.R script
airmaster <- read_feather("airmasterk.feather")

#creates new hour and date columns, and a column for the log10 of the pm2.5 calculation
airmasterk <- airmasterk %>%
  mutate(hour = hour(Time), date = as_date(Time),
         lnpm2.5 = log10(pm2.5)
  )

#groups be the date and does calculations for the error bars
airmasterk <- airmasterk %>%
  group_by(date) %>% #group by the date
  summarise(daily_mean = mean(pm2.5), #daily pm2.5 mean
            daily_lnmean = mean(lnpm2.5), #daily natural log pm2.5
            sdlnpm2.5 = sd(lnpm2.5), #standard deviation of daily natural log pm2.5
            selnpm2.5 = (sdlnpm2.5)/sqrt(1440), #standard error of daily natural log pm2.5
            sepm2.5 = 10 ^ selnpm2.5
  ) %>%
  na.omit()

#creates column for a lubridate version of the date 
airmasterk <- airmasterk %>%
  mutate(
    YMD = ymd(date)
  )

#generates the February point plot with error bars
ggplot(subset(airmasterk, month(YMD) == 2),
       aes(YMD,y = daily_mean)) +
  geom_point(aes(y = daily_mean), color = "red", size= 1) +
  geom_errorbar(aes(ymin = daily_mean - sepm2.5, ymax = daily_mean + sepm2.5), size = .2, width = .5,alpha = .3) +
  geom_line(aes(y = daily_mean)) +
  labs(
    y = "PM2.5",
    x = "Date",
    title = paste(
      "February 2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 Daily Averages"
    ))
