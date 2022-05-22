library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)


# This file should be the latest and greatest master data set from particle counter
# Note that the current "master" set we have looks like it underwent some processing
masterdata <- "Copy of pcmaster_ktown_1-18.csv"


df <- read_csv(masterdata)


# pull in smaller extra dataset we got from Marco and reformat
end_df<- read_csv("Copy of 01 2021-01-28-16-07-41.csv", skip = 2) %>%
  mutate(
    ObsID = NA,
    week = NA,
    weekday = NA,
    check = NA
  ) %>%
  rename(
    c0.3 =  `0.3 (Counts)`, 
    c0.5 = `0.5 (Counts)`,
    c0.7 = `0.7 (Counts)`,
    c1.0 = `1.0 (Counts)`,
    c2.0 = `2.0 (Counts)`,
    c3.0 = `3.0 (Counts)`,
    c5.0 = `5.0 (Counts)`,
    c10.0 = `10.0 (Counts)`,
    Temp = `AT(C)`,
    RH = `RH(%)`
  )

# combine the two sets

df <- rbind(df, end_df)

# Change time strings to datetime type
df <- mutate(df, Time= parse_date_time(Time,"mdYHM"))

# Remove problem chunks, per review with Eli/Mikala:
df_remove <- df %>% 
  filter(
    #work in progress
    (Time > ymd_hms("2020-01-16 04:00:00") & Time < ymd_hms("2020-01-16 08:00:00")) |
    (Time > ymd_hms("2020-01-18 12:00:00") & Time < ymd_hms("2020-01-19 06:00:00")) |
    (Time > ymd_hms("2020-01-26 00:00:00") & Time < ymd_hms("2020-01-26 09:00:00")) |
    (Time > ymd_hms("2020-02-10 00:00:00") & Time < ymd_hms("2020-02-10 08:00:00")) |
    (Time > ymd_hms("2020-02-11 00:00:00") & Time < ymd_hms("2020-02-11 08:00:00")) |
    (Time > ymd_hms("2020-07-01 17:00:00") & Time < ymd_hms("2020-07-01 23:59:00")) |
    (Time > ymd_hms("2020-08-17 02:00:00") & Time < ymd_hms("2020-08-17 23:59:00")) |
    (Time > ymd_hms("2020-08-24 13:00:00") & Time < ymd_hms("2020-08-24 14:30:00")) |
    (Time > ymd_hms("2020-08-27 12:00:00") & Time < ymd_hms("2020-08-29 23:59:00")) |
    (Time > ymd_hms("2020-12-17 00:00:00") & Time < ymd_hms("2020-12-17 06:00:00")) |
    (Time > ymd_hms("2020-02-06 15:00:00") & Time < ymd_hms("2020-02-07 06:00:00")) |
    (Time > ymd_hms("2020-02-11 11:00:00") & Time < ymd_hms("2020-02-11 14:00:00")) |
    (Time > ymd_hms("2020-10-29 00:00:00") & Time < ymd_hms("2020-10-29 08:00:00")) |
    (Time > ymd_hms("2020-11-13 18:00:00") & Time < ymd_hms("2020-11-13 23:59:00")) 
  )

df <- df %>%
  anti_join(df_remove)

# Filtering for negative values for counts, and values over 900,000,000
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


# ### Create a seperate dataframe of outliers for this dataset
# outlier_factor <- 1500 #using bigger multiplier because traditional 1.5 results in too many datapoints because of the distribution
# c0.3outlier <- fivenum(df$c0.3)[4] +  ((fivenum(df$c0.3)[4] - fivenum(df$c0.3)[2]) * outlier_factor)
# c0.5outlier <- fivenum(df$c0.5)[4] +  ((fivenum(df$c0.5)[4] - fivenum(df$c0.5)[2]) * outlier_factor)
# c0.7outlier <- fivenum(df$c0.7)[4] +  ((fivenum(df$c0.7)[4] - fivenum(df$c0.7)[2]) * outlier_factor)
# c1.0outlier <- fivenum(df$c1.0)[4] +  ((fivenum(df$c1.0)[4] - fivenum(df$c1.0)[2]) * outlier_factor)
# c2.0outlier <- fivenum(df$c2.0)[4] +  ((fivenum(df$c2.0)[4] - fivenum(df$c2.0)[2]) * outlier_factor)
# c5.0outlier <- fivenum(df$c5.0)[4] +  ((fivenum(df$c5.0)[4] - fivenum(df$c5.0)[2]) * outlier_factor)
# c10.0outlier <- fivenum(df$c10.0)[4] +  ((fivenum(df$c10.0)[4] - fivenum(df$c10.0)[2]) * outlier_factor)
# 
# outliers <- df %>%
#   filter((c0.3 > c0.3outlier) | (c0.5 > c0.5outlier) | (c0.7 > c0.7outlier) | (c1.0 > c1.0outlier) | (c2.0 > c2.0outlier) | 
#            (c5.0 > c5.0outlier) | (c10.0 > c10.0outlier)) 
#   
# write_csv(outliers, "outliers_metone_amnc.csv")

# Using average diameters for each bin to get count totals by size
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
  ) %>%
  #set any negative counts to zero
  mutate(
    tot0.4count = ifelse(tot0.4count >= 0, tot0.4count, 0), 
    tot0.6count = ifelse(tot0.6count >= 0, tot0.6count, 0), 
    tot0.85count = ifelse(tot0.85count >= 0, tot0.6count, 0), 
    tot1.5count = ifelse(tot1.5count >= 0, tot0.6count, 0), 
    tot2.5count = ifelse(tot2.5count >= 0, tot0.6count, 0), 
    tot4.0count = ifelse(tot4.0count >= 0, tot0.6count, 0),  
    tot7.5count = ifelse(tot7.5count >= 0, tot0.6count, 0), 
    tot20count = ifelse(tot20count >= 0, tot0.6count, 0), 
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

# Calculating volumes
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

# Estimating EPA thresholds
# 1)Sum the volume of particles of different sizes (in cubic microns) 
# 2)convert to cubic cm, dividing by 1000000000000
# 3) multiply by 1.65 to get the weight per cubic cm of particle in grams. 
# 4) Then multiply by 1000000 to convert mass in grams to micrograms
# 5) Convert the 1 lpm sampled to cubic meters (multiplying by 1000),
airmasterk <- airmasterk %>% rowwise() %>%
  mutate(
    pm2.5 = sum(c(totvol0.4,totvol0.6,totvol0.85,totvol1.5,totvol2.5)) / 1000000000000 * 1.65 *1000000 *1000,
    pm10 = sum(c(totvol0.4,totvol0.6,totvol0.85,totvol1.5,totvol2.5,totvol4.0,totvol7.5)) / 1000000000000 * 1.65 *1000000 *1000
  )

# Calculating mass for individual bins too, with the same formula that we used for PM2.5 and PM10
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
negatives <- airmasterk %>%
  filter(pm2.5 < 0)
view(negatives)
# # 
# #pulls all the observations where pm2.5 is above 100 into a dataframe
# highpm2.5 <- airmasterk %>%
#   filter(pm2.5 > 100)
# view(highpm2.5)



# Adjusting for Flow(lpm)
airmasterk <- airmasterk %>% 
  mutate(
    pm2.5 = pm2.5 * `Flow(lpm)`,
    pm10 = pm10  * `Flow(lpm)`
  )

#set any negative PM to zero
mutate(
  pm2.5 = ifelse(pm2.5 >= 0, pm2.5, 0), 
  pm10 = ifelse(pm10 >= 0, pm10, 0)
)



# Saving to a feather file so that other scripts can use prepared tibble.
write_feather(airmasterk,"airmasterk.feather")


### Looking at Outlier Issues: ###

# # Temp plot to take a look at sections that might get cut 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 1 & day(Time) > 25 & day(Time) < 27), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 1 & day(Time) > 1 & day(Time) < 17 & hour(Time) > 4 & hour(Time) < 8), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 1 & ((day(Time) == 18 & hour(Time) > 12) | (day(Time) == 19 & hour(Time) < 6))), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# # Leaving this one ? Doesn't show up in Lorraine or Julie's outdoor unit, but only seems like PM10 going wild:
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 2 & ((day(Time) == 6 & hour(Time) > 15) | (day(Time) == 7 & hour(Time) < 6))), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 2 & day(Time) > 9 & day(Time) < 11), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 2 & (day(Time) == 11 & (hour(Time) > 10 & hour(Time) < 14))), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 7 & (day(Time) == 1 & (hour(Time) < 24))), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 8 & ((day(Time) == 17 & hour(Time) > 1))), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 8 & ((day(Time) == 24 & hour(Time) > 0) & hour(Time) < 16)), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 8 & day(Time) > 26 & day(Time) < 30), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 11 & day(Time) > 12 & day(Time) < 14), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2020 & month(Time) == 12 & day(Time) > 16 & day(Time) < 18), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)
# 
# ggplot(subset(airmasterk,year(Time) == 2021 & month(Time) == 2 & day(Time) > 0 & day(Time) < 32), aes(x = Time)) +
#   geom_point(aes(y = pm10), color = "blue", size= .3) +
#   geom_point(aes(y = pm2.5), color = "red", size= .3)