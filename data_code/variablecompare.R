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


ggplot(airmasterk,aes(Temp, pm2.5)) +
  geom_hex(bins = 70) +
  geom_smooth() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(-12,60)) +
  labs(
    title = paste(
      "PM2.5 Values in Relation to Temp (C)"
    ))

# ggplot(data = airmasterk) +
#   geom_point(mapping = aes(x = Temp, y = pm2.5), 
#              color = "blue", size = .3, alpha = .05) +
#   scale_y_continuous(limits = c(0,100)) +
#   scale_x_continuous(limits = c(-12,60)) +
#   labs(
#     title = paste(
#       "PM2.5 Values in Relation to Temp (C)"
#     ))


ggplot(airmasterk,aes(RH, pm2.5)) +
  geom_hex(bins = 70) +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,100)) +
  labs(
    title = paste(
      "PM2.5 Values in Relation to RH"
    ))

# ggplot(data = airmasterk) +
#   geom_point(mapping = aes(x = RH, y = pm2.5), 
#              color = "red", size = .3, alpha = .05) +
#   scale_y_continuous(limits = c(0,100)) +
#   scale_x_continuous(limits = c(0,100)) +
#   labs(
#     title = paste(
#       "PM2.5 Values in Relation to RH"
#     ))


ggplot(data =airmasterk) +
  geom_histogram(mapping = aes(x = pm2.5),bins = 1)


qplot(airmasterk$Temp, geom = "histogram",
      binwidth = 5,
      main = "Histogram for Temp(C)", 
      xlab = "Temp (C)",  
      fill=I("blue"), 
      col=I("white"),)

qplot(airmasterk$RH, geom = "histogram",
      binwidth = 5,
      main = "Histogram for RH", 
      xlab = "RH %",  
      fill=I("blue"), 
      col=I("white"),)





















