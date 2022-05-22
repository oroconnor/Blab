library(tidyverse)
library(lubridate)
library(ggplot2)

newdata <- "pcmaster_ktown_12-17.csv"

df <- read_csv(newdata)

#change time strings to datetime type
df<-mutate(df, Time= parse_date_time(Time,"mdYHM"))

df <- df %>%
  mutate(hour = hour(Time), date = as_date(Time))

#using average diameters for each bin to get count totals by size
# airmasterk <- df %>%
#   mutate( 
#     tot0.4count = c0.3 - c0.5,
#     tot0.6count = c0.5 - c0.7,
#     tot0.85count = c0.7 - c1.0,
#     tot1.5count = c1.0 - c2.0,
#     tot2.5count = c2.0 - c3.0,
#     tot4.0count = c3.0 - c5.0,
#     tot7.5count = c5.0 - c10.0,
#     tot20count = c10.0,
#     )

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

#using 1.65 g/cm3 as density as per paper, calculating mass for each count bin, resulting in ug/m3

#calculating volumes
# airmasterk <- df %>%
#   mutate( 
#     v0.3 = (0.00498891615454387*c0.3),
#     v0.5 = (0.0346540661567689*c0.5),
#     v0.7 = (0.115661158466949*c0.7),
#     v1.0 = (0.321555098064305*c1.0),
#     v2.0 = (1.93100800251331*c2.0),
#     v3.0 = (34.6540661567689*c1.0),
#     v5.0 = (115.661158466949*c5.0),
#     v10.0 = (1767.14586764426*c10.0),
#   )

#estimating EPA thresholds
#1)Sum the volume of particles of different sizes (in cubic microns) 2)Then you need to 
#both convert to cubic cm, which involves dividing by 1000000000000, times by 1.65 to get the weight 
#per cubic cm of particle
#and convert the 1 lpm sampled to 
#cubic meters (multiplying by 1000), 
# airmasterk <- airmasterk %>% rowwise() %>%
#   mutate( 
#     pm2.5 = sum(c(v0.3,v0.5,v0.7,v1.0,v2.0)) / 1000000000000 * 1.65 *1000000 *1000,
#     pm10 = sum(c(v0.3,v0.5,v0.7,v1.0,v2.0,v3.0,v5.0,v10.0)) / 1000000000000 * 1.65 *1000000 *1000
#   )
airmasterk <- airmasterk %>% rowwise() %>%
  mutate(
    pm2.5 = sum(c(m0.4,m0.6,m0.85,m1.5,m2.5)),
    pm10 = sum(c(m0.4,m0.6,m0.85,m1.5,m2.5,m4.0,m7.5,m20)) 
  )

airmasterk <- airmasterk %>%
  group_by(date,hour) %>% #group by the date and hour columns
  summarise(hourlypm2.5 = mean(pm2.5),hourlypm10 = mean(pm10), hourly_temp = mean(Temp) ) %>%
  na.omit()



airmasterk <- airmasterk %>%
  mutate(YMDH = ymd_h(paste(date,hour)))

#view(airmasterk)
 

ggplot(airmasterk,
       aes(YMDH,y)) +
  geom_point(aes(y = hourlypm2.5), color = "red", size= .4) +
  geom_hline(yintercept=35, color="orange", lty=2) +
  scale_y_continuous(limits = c(0,100)) +
  labs(
    y = "PM2.5",
    x = "Date and Hour",
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 Hourly Averages"
    ))




