library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

#This file should be the latest and greatest master data set from particle counter
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

#Saving to a feather file so that other scripts can use prepared tibble. 
write_feather(airmasterk,"airmasterk.feather")
