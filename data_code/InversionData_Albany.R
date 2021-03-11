library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)
library(zoo)

#Imports Albany soundings for Jan 16 2020 to Jan 15 2021
inversiondata <- "KALY_2020011600_2021011500.txt"

#import airmasterk
#read in the tibble prepared by airmasterk.R script
airmasterk <- read_feather("airmasterk.feather")

df <- read_csv(inversiondata)

#filtering date, excluding altitudes with no temp readings
df1 <- df %>%
  filter(
    tmpc != "M",
  )

df1 <- df1 %>%
  select(station, validUTC, height_m, tmpc)

df1 <- df1 %>%
  mutate(
    tmpc = as.numeric(tmpc),
    #level gets assigned in the while loop below that iterates through observations
    level = NULL
    )

#ignore this chart for now
ggplot(df1, aes(x = validUTC,y = tmpc, color = height_m)) +
  geom_point(size = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  #scale_x_datetime(date_breaks = "6 hours", date_labels = "%b %d %l %p") +
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(
    x = "Date/Time",
    y = "Temperature (C)",
    title = paste(
    "Albany Atmosphere Temperature Readings"
    ),
    subtitle = paste(
      "December 2020"
    ),
    caption =  "data source: https://mesonet.agron.iastate.edu/archive/raob/" 
    )

#below here is the first attempt to iterate through these dates
  
min_date <- min(as_datetime(df1$validUTC))
max_date <- max(as_datetime(df1$validUTC))

i <- min_date
#empty df2 that will be added to after labeling in the while loop below
df2 <- tibble(station = character(), validUTC = ymd_hms(), height_m = numeric(), tmpc = numeric())


while (i < max_date) {
  #iterating through the observations
  tempdf <- df1 %>%
    filter(
      validUTC == i
    )
  level_order <- 1
  
  
  #label each reading with the altitude level
  #doesn't actual look at altitude right now, just labels the order that they are
  #in the data for each observation
  if (nrow(tempdf) == 0) i <- i + dhours(12)
  else {
    for (j in 1:nrow(tempdf)) {
      tempdf[j,"level"] <- level_order
      level_order <- level_order + 1
    }    
    
    #this is the where the inversion label is applied
    ifelse(
      tempdf[1,"tmpc"] < tempdf[2,"tmpc"] & tempdf[2,"tmpc"] < tempdf[3,"tmpc"], 
      tempdf$inversion <- 1,
      tempdf$inversion <- 0
      )
    
    #append each tempdf to df2 
    df2 <- full_join(df2,tempdf)
  
  #increment to the next reading
  i <- i + dhours(12)
  }
}

#collapse df2 to just one entry for each Albany sounding observation
#and a 0/1 label for inversion for each observation
df2 <- df2 %>%
  group_by(validUTC) %>%
  summarise(
    inversion = mean(inversion)
  ) %>%
  mutate( 
    Time = validUTC
    ) %>%
  select(
    Time,inversion
  )

imasterk <- full_join(airmasterk, df2) %>%
  select(
    Time,
    pm2.5,
    inversion
  )

imasterk <- na.omit(imasterk)
imasterk$inversion <- as.factor(imasterk$inversion)


#All the datapoints that overlap (just at midnight and noon)
ggplot(data = imasterk,
       aes(x = Time,y = pm2.5, color = inversion)) +
  geom_point(aes(y = pm2.5), size= 1.4, shape = 1) +
  #geom_errorbar(aes(ymin = daily_mean - sepm2.5, ymax = daily_mean + sepm2.5), size = .2, width = 1,alpha = .3) +
  #geom_smooth(se = FALSE) +
  #geom_hline(yintercept=35, color="orange", lty=2) +
  #annotate("text", x = as.Date("2020-11-15", "%Y-%m-%d"),  y = 37, label = "EPA 24-hr Avg. Threshold", size = 3) +
  #geom_line(aes(y = daily_mean), color = "red") +
  #scale_x_date(limits = as.Date(c("2020-01-16","2021-01-16")), date_breaks = "months", date_labels = "%b") +
  labs(
    y = "PM2.5 - micrograms/cubic meter",
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "with inversion labels from Albany"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#same plot but subsetting the inversion 1 vs 0
ggplot(imasterk, aes(x=Time,y = pm2.5)) +
  geom_point(data=subset(imasterk,inversion==1), color = "red", size= 1) +
  geom_point(data=subset(imasterk,inversion==0), color = "blue", shape= 1, size= 1, alpha = .5) +
  #geom_smooth(data=subset(imasterk,inversion==1), color = "red") +
  #geom_smooth(data=subset(imasterk,inversion==0), color = "blue") +
  #scale_y_continuous(limits = c(0,42)) +
  labs(
    y = "PM2.5 - micrograms/cubic meter",
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "with inversion labels from Albany, RED = inversion"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


#boxplot

ggplot(imasterk, aes(x=inversion, y=pm2.5, color=inversion)) +
  geom_boxplot() +
  labs(
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "with inversion labels from Albany"
      
  ))

# below attempts to join full data set and impute inversion labels 
amasterk <- full_join(airmasterk, df2) 


amasterk <- amasterk %>%
  select(
    Time,
    pm2.5,
    inversion
  ) %>%
  fill(inversion) #just filling forward for now



amasterk <- amasterk %>%
  mutate(hour = hour(Time), date = as_date(Time), hourdate= update(date, hour = hour(Time))
       )


amasterk <- amasterk %>%
  group_by(hourdate) %>% #group by the date and hour
  summarise(hourly_pm2.5 = mean(pm2.5), #hourly pm2.5 mean
            hourly_inversion = mean(inversion)
  ) %>%
  na.omit()


#same plot but subsetting the inversion 1 vs 0
ggplot(amasterk, aes(x=hourdate,y = hourly_pm2.5)) +
  geom_point(data=subset(amasterk,hourly_inversion==1), color = "red", size= 1) +
  geom_point(data=subset(amasterk,hourly_inversion==0), color = "blue", shape= 1, size= 1, alpha = .5) +
  geom_smooth(data=subset(amasterk,hourly_inversion==1), color = "red") +
  geom_smooth(data=subset(amasterk,hourly_inversion==0), color = "blue") +
  scale_y_continuous(limits = c(0,50)) +
  labs(
    y = "PM2.5 - micrograms/cubic meter",
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "with inversion labels from Albany"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))



###########################
#scrap pieces from trying different ways to fill values backward/forward
#currently mostly garbage

i=1
while (i < nrow(amasterk)) {
    #iterating through the observations, for amasterk
  if ((hour(amasterk$Time) > 11 && hour(amasterk$Time) < 18) || (hour(amasterk$Time) >= 0 && hour(amasterk$Time) < 6)) {
    if (i < 286) a <- 0
    else a <- i - 285
    amasterk[i,"inversion"] <- sapply(amasterk[a:i,"inversion"],mean,na.rm = TRUE)

  }
  else {
  amasterk[i,"inversion"] <-  sapply(amasterk[i:(i+285),"inversion"], mean,na.rm = TRUE)

  }
  
  #increment to the next reading
  i <- i + 1
}

i=1
for (i in 1:nrow(amasterk)) {
  amasterk[i,"inversion"] <- i
  i <- i + 1
}



i=1
mean(amasterk[i:(i+285),"inversion"],na.rm = TRUE)

b <- amasterk[1:12,"inversion"]
mean(as.numeric(b),na.rm = TRUE)
lapply(b, mean, na.rm = TRUE)