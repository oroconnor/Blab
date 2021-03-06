#Creates airmasterk from pcmaster_ktown.CSV and creating scatterplot for 2020 of daily mean for PM2.5, with error bars

library(tidyverse)
library(lubridate)
library(ggplot2)

newdata <- "pcmaster_ktown_12-17.csv"

df <- read_csv(newdata)

#change time strings to datetime type
df<-mutate(df, Time= parse_date_time(Time,"mdYHM"))

#using average diameters for each bin to get count totals by size
airmasterk <- df %>%
  mutate(
    tot0.4count = c0.3 - c0.5, 
    tot0.6count = c0.5 - c0.7,
    tot0.85count = c0.7 -c1.0, 
    tot1.5count = c1.0 - c2.0, 
    tot2.5count = c2.0 - c3.0, 
    tot4.0count = c3.0 - c5.0, 
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
    
airmasterk <- airmasterk %>%
  mutate(hour = hour(Time), date = as_date(Time),
  lnpm2.5 = log10(pm2.5)
  )

#grouping by daily means and doing calculations for error bars
airmasterk <- airmasterk %>%
  group_by(date) %>% #group by the date 
  summarise(daily_mean = mean(pm2.5), #daily pm2.5 mean
            daily_lnmean = mean(lnpm2.5), #daily natural log pm2.5
            sdlnpm2.5 = sd(lnpm2.5), #standard deviation of daily natural log pm2.5
            selnpm2.5 = (sdlnpm2.5)/sqrt(1440), #standard error of daily natural log pm2.5
            sepm2.5 = 10 ^ selnpm2.5
            ) %>%
  na.omit()

airmasterk <- airmasterk %>%
  mutate(
    YMD = ymd(date)
    )

#removed negative values
airmasterk <- airmasterk %>% filter(daily_mean >= 0
) 

ggplot(data = airmasterk,
       aes(x = YMD,y = daily_mean)) +
  geom_point(aes(y = daily_mean), color = "red", size= 1) +
  geom_errorbar(aes(ymin = daily_mean - sepm2.5, ymax = daily_mean + sepm2.5), size = .2, width = 1,alpha = .3) +
  #geom_line(aes(y = daily_mean), color = "red") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  labs(
    y = "PM2.5",
    x = "Date",
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 Daily Averages"
    ))

