### Demonstrates Summary Plots

library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)
library(PerformanceAnalytics)
library(car)
library(psych)
library(fitdistplus)
library(pastecs)
library(openair)
library(corrplot)

#importing my dataframe and selecting for the numeric columns
#you can ignore this and import your own data and assign it to 
#df as a dataframe
df <- read_feather("airmasterk.feather") %>%
  filter(
    month(Time) == 1,
    day(Time) < 5
  ) %>%
  select(
 #   c0.3,c0.5,c0.7,c1.0,Temp,RH,pm2.5
    c0.3,c0.5,c0.7,c1.0,Temp,RH,pm2.5  
  )

df_wDate <- read_feather("airmasterk.feather") %>%
  filter(
    year(Time) == 2020
  ) %>%
  mutate(
    date = Time
  ) %>%
  select(
    #   c0.3,c0.5,c0.7,c1.0,Temp,RH,pm2.5
    date,c0.3,c0.5,c0.7,c1.0,Temp,RH,pm2.5  
  ) 

df_wDate <- df_wDate[-36781,]


#quick look at dataframe
str(df)

#similar quick look 
glimpse(df)

### Summary statistics:

#Summary statistics of the variables of df
#min,quartiles, median, mean, max
summary(df)

#Summary stats (psych package): n, mean, sd, median, trimmed, mad, min, max, range, skew, kurtosis, se
describe(df)

#Summary stats (pastecs pacakage):

stat.desc(df)

### Univariate plots:

summaryPlot(df_wDate)

### Bivariate plots:

#correlation plot  w/ duplicate scatterplots
scatterplotMatrix(df)

#correlation plot w/ correlation metrics too
chart.Correlation(df)

#correlation plot
pairs.panels(df)

#correlation plot
corrplot(cor(df), method = "ellipse")



