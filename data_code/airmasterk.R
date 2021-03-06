library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

#This file should be the latest and greatest master data set from particle counter
newdata <- "Copy of pcmaster_ktown_1-18.csv"

df <- read_csv(newdata)

#change time strings to datetime type
df<-mutate(df, Time= parse_date_time(Time,"mdYHM"))

#filtering for negative values for counts, and values over 900,000,000
df <- df %>% 
  filter(
    c0.3 >= 0 & c0.3 <= 900000000,
    c0.5 >= 0 & c0.5 <= 900000000,
    c0.7 >= 0 & c0.7 <= 900000000,
    c1.0 >= 0 & c1.0 <= 900000000,
    c2.0 >= 0 & c2.0 <= 900000000,
    c3.0 >= 0 & c3.0 <= 900000000,
    c5.0 >= 0 & c5.0 <= 900000000,
    c10.0 >= 0 & c10.0 <= 900000000,
    Alarms == 0
  )



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
    tot20count = c10.0
  )



# Sphere volume constants. For each bin, we're calculating the amount of 
#volume in one particle of the midpoint size of the bin. V = 4/3*pi*r^3
vol0.4 = (4/3) * pi * (.4/2)^3
vol0.6 = (4/3) * pi * (.6/2)^3
vol0.85 = (4/3) * pi * (.85/2)^3
vol1.5 = (4/3) * pi * (1.5/2)^3
vol2.5 = (4/3) * pi * (2.5/2)^3
vol4.0 = (4/3) * pi * (4.0/2)^3
vol7.5 = (4/3) * pi * (7.5/2)^3
vol20 = (4/3) * pi * (20/2)^3

#calculating volumes
airmasterk <- airmasterk %>%
   mutate(
     totvol0.4 = (vol0.4*tot0.4count),
     totvol0.6 = (vol0.6*tot0.6count),
     totvol0.85 = (vol0.85*tot0.85count),
     totvol1.5 = (vol1.5*tot1.5count),
     totvol2.5 = (vol2.5*tot2.5count),
     totvol4.0 = (vol4.0*tot4.0count),
     totvol7.5 = (vol7.5*tot7.5count),
     totvol20 = (vol20*tot20count)
    )


#estimating EPA thresholds
#1)Sum the volume of particles of different sizes (in cubic microns) 
#2)convert to cubic cm, dividing by 1000000000000
#3) multiply by 1.65 to get the weight per cubic cm of particle in grams. 
#4) Then multiply by 1000000 to convert mass in grams to micrograms
#5) Convert the 1 lpm sampled to cubic meters (multiplying by 1000),
airmasterk <- airmasterk %>% rowwise() %>%
  mutate(
    pm2.5 = sum(c(totvol0.4,totvol0.6,totvol0.85,totvol1.5,totvol2.5)) / 1000000000000 * 1.65 *1000000 *1000,
    pm10 = sum(c(totvol0.4,totvol0.6,totvol0.85,totvol1.5,totvol2.5,totvol4.0,totvol7.5)) / 1000000000000 * 1.65 *1000000 *1000
  )

#calculating mass for individual bins too, with the same formula that we used for PM2.5 and PM10
airmasterk <- airmasterk %>%
  mutate(
    mass0.4 =  totvol0.4 / 1000000000000 * 1.65 *1000000 *1000,
    mass0.6 =  totvol0.6 / 1000000000000 * 1.65 *1000000 *1000,
    mass0.85 =  totvol0.85 / 1000000000000 * 1.65 *1000000 *1000,
    mass1.5 =  totvol1.5 / 1000000000000 * 1.65 *1000000 *1000,
    mass2.5 =  totvol2.5 / 1000000000000 * 1.65 *1000000 *1000,
    mass4.0 =  totvol4.0 / 1000000000000 * 1.65 *1000000 *1000,
    mass7.5 =  totvol7.5 / 1000000000000 * 1.65 *1000000 *1000,
    mass20 =  totvol20 / 1000000000000 * 1.65 *1000000 *1000
  )



# #pulls all the observations where the Flow isn't exactly one
# lowflow <- airmasterk %>%
#   filter(`Flow(lpm)` != 1)
# view(lowflow)
# 
# #pulls all the observations where pm2.5 is negative into a dataframe
# negatives <- airmasterk %>%
#   filter(pm2.5 < 0)
# view(negatives)
# # 
# #pulls all the observations where pm2.5 is above 100 into a dataframe
# highpm2.5 <- airmasterk %>%
#   filter(pm2.5 > 100)
# view(highpm2.5)

#adjusting for Flow(lpm)
airmasterk <- airmasterk %>% 
  mutate(
    pm2.5 = pm2.5 * 'Flow(lpm)',
    pm10 = pm10  * 'Flow(lpm)'
  )




#Saving to a feather file so that other scripts can use prepared tibble.
write_feather(airmasterk,"airmasterk.feather")
