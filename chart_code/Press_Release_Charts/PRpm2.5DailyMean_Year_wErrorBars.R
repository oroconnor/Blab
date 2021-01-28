library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

#read in the tibble prepared by airmasterk.R script
airmasterk <- read_feather("airmasterk.feather")

#creates new hour and date columns, and a column for the log10 of the pm2.5 calculation
airmasterk <- airmasterk %>%
  mutate(hour = hour(Time), date = as_date(Time),
  )

#groups be the date and does calculations for the error bars
airmasterk <- airmasterk %>%
  group_by(date) %>% #group by the date
  summarise(daily_mean = mean(pm2.5), #daily pm2.5 mean
            sdpm2.5 = sd(pm2.5), #standard deviation of daily pm2.5
            sepm2.5 = (sdpm2.5)/sqrt(1440), #standard error of daily pm2.5
  ) %>%
  na.omit()

#creates column for a lubridate version of the date 
airmasterk <- airmasterk %>%
  mutate(
    YMD = ymd(date)
  )

#generates the year point plot with error bars
ggplot(data = airmasterk,
       aes(x = YMD,y = daily_mean)) +
  geom_point(aes(y = daily_mean), color = "red", size= 1.4) +
  geom_errorbar(aes(ymin = daily_mean - sepm2.5, ymax = daily_mean + sepm2.5), size = .2, width = 1,alpha = .3) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept=35, color="orange", lty=2) +
  annotate("text", x = as.Date("2020-11-15", "%Y-%m-%d"),  y = 37, label = "EPA 24-hr Avg. Threshold", size = 3) +
  #geom_line(aes(y = daily_mean), color = "red") +
  scale_x_date(limits = as.Date(c("2020-01-16","2021-01-16")), date_breaks = "months", date_labels = "%b") +
  labs(
    y = "PM2.5 - microns/cubic meter",
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 Daily Averages"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
