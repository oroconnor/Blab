library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)
library(wesanderson)
library(PerformanceAnalytics)

###Prep Julie Data
mydir1 = "Julie_Outdoor_Data"
myfiles1 = list.files(path=mydir1, pattern="*.csv", full.names=TRUE)

julie_data <-myfiles1 %>%
  map_dfr(read_csv, col_types = "Tcdddddddddddddddddddd")    # read in all the files individually, using


#combine data from different csv files with differing column names
julie_data <- julie_data %>%
  unite("temp",`temperature[n/a]`,`temperature[degC]`, na.rm = TRUE) %>%
  unite("RH",`humidity[n/a]`,`humidity[percent]`, na.rm = TRUE) %>%
  unite("pressure",`pressure[n/a]`,`pressure[Pa]`, na.rm = TRUE) %>%
  unite("co2",`co2[n/a]`,`co2[ppm]`, na.rm = TRUE) %>%
  unite("pm10p0",`pm10p0[n/a]`,`pm10p0[ug/m^3]`, na.rm = TRUE) %>%
  unite("pm1p0",`pm1p0[ug/m^3]`,`pm1p0[ug/m^3]`, na.rm = TRUE) %>%
  unite("pm2p5",`pm2p5[n/a]`,`pm2p5[ug/m^3]`, na.rm = TRUE) 

julie_data <- julie_data %>%
  select(
    time, temp, RH, pressure, co2, pm10p0, pm1p0, pm2p5
  )
  
#convert columns to numeric
julie_data$temp <- as.numeric(julie_data$temp)
julie_data$RH <- as.numeric(julie_data$RH)
julie_data$pressure <- as.numeric(julie_data$pressure)
julie_data$co2 <- as.numeric(julie_data$co2)
julie_data$pm10p0 <- as.numeric(julie_data$pm10p0)
julie_data$pm1p0 <- as.numeric(julie_data$pm1p0)
julie_data$pm2p5 <- as.numeric(julie_data$pm2p5)



### Prep Lorraine Data

mydir2 = "Lorraine_Outdoor_Data"
myfiles2 = list.files(path=mydir2, pattern="*.csv", full.names=TRUE)

lorraine_data <- myfiles2 %>%
  map_dfr(read_csv, col_types = "Tcdddddddddddddddddddd")    # read in all the files individually, using

#combine data from different csv files with differing column names
lorraine_data <- lorraine_data %>%
  unite("temp",`temperature[n/a]`,`temperature[degC]`, na.rm = TRUE) %>%
  unite("RH",`humidity[n/a]`,`humidity[percent]`, na.rm = TRUE) %>%
  unite("pressure",`pressure[n/a]`,`pressure[Pa]`, na.rm = TRUE) %>%
  unite("co2",`co2[n/a]`,`co2[ppm]`, na.rm = TRUE) %>%
  unite("pm10p0",`pm10p0[n/a]`,`pm10p0[ug/m^3]`, na.rm = TRUE) %>%
  unite("pm1p0",`pm1p0[ug/m^3]`,`pm1p0[ug/m^3]`, na.rm = TRUE) %>%
  unite("pm2p5",`pm2p5[n/a]`,`pm2p5[ug/m^3]`, na.rm = TRUE) 

lorraine_data <- lorraine_data %>%
  select(
    time, temp, RH, pressure, co2, pm10p0, pm1p0, pm2p5
  )

#convert columns to numeric
lorraine_data$temp <- as.numeric(lorraine_data$temp)
lorraine_data$RH <- as.numeric(lorraine_data$RH)
lorraine_data$pressure <- as.numeric(lorraine_data$pressure)
lorraine_data$co2 <- as.numeric(lorraine_data$co2)
lorraine_data$pm10p0 <- as.numeric(lorraine_data$pm10p0)
lorraine_data$pm1p0 <- as.numeric(lorraine_data$pm1p0)
lorraine_data$pm2p5 <- as.numeric(lorraine_data$pm2p5)





#read in the tibble prepared by andy.R
eggmasterk <- read_feather("eggmasterk.feather")


eggmasterk <- eggmasterk %>%
  select(
    `time`,`temperature[degC]`,`humidity[percent]`, `co2[ppm]`,`pm10p0[ug/m^3]`,`pm2p5[ug/m^3]`
  ) %>%
  rename(
    pm10p0 = `pm10p0[ug/m^3]`,
    pm2p5 = `pm2p5[ug/m^3]`,
    co2 = `co2[ppm]`,
    temp= `temperature[degC]`,
    RH = `humidity[percent]`
  ) 

eggmasterk$pm10p0 <- as.numeric(eggmasterk$pm10p0)
eggmasterk$pm2p5 <- as.numeric(eggmasterk$pm2p5)
eggmasterk$co2 <- as.numeric(eggmasterk$co2)
eggmasterk$temp <- as.numeric(eggmasterk$temp)
eggmasterk$RH <- as.numeric(eggmasterk$RH)

eggmasterk<-mutate(eggmasterk, time= parse_datetime(time))

#Add sensor label
eggmasterk$sensor <- "Andy"

outdoor <- inner_join(lorraine_data,julie_data,by="time", suffix = c(".lorraine", ".julie")) %>% 
  inner_join(eggmasterk,by="time") %>%
  na.omit()




#Time Series plot
ggplot(outdoor, aes(time,y)) +
  geom_point(aes(y = pm2p5.lorraine), size= .3, color = "red", alpha =.4) +
  geom_point(aes(y = pm2p5.julie), size= .3, color = "blue", alpha =.4) +
  geom_point(aes(y = pm2p5), size= .3, color = "green", alpha =.4) +
  geom_smooth(aes(y = pm2p5.lorraine), color = "red", se =FALSE) +
  geom_smooth(aes(y = pm2p5.julie),color = "blue", se =FALSE) +
  geom_smooth(aes(y = pm2p5),color = "green", se =FALSE) +
  theme_classic() +
  labs(
    y = "pm2.5",
    x = NULL,
    title = paste(
      "Comparing Outdoor Eggs"
    ),
    subtitle = paste(
      "Andy = Green, Julie = Blue, Lorraine = Red"
    )) +
scale_y_continuous(limits = c(0,50))



#boxplots
boutdoor <- gather(outdoor,`Sensor`, pm2.5,c(pm2p5.julie,pm2p5.lorraine,pm2p5))%>%
  mutate(
    month = month(time, label=TRUE)
  )

ggplot(boutdoor,aes(x=month,pm2.5, fill = `Sensor`)) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom = "errorbar") +
  labs(
    y = expression(PM2.5 - (μg/~m^3)),
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "without outliers"
    )) +
  scale_fill_manual(labels = c("Andy","Julie","Lorraine"),values=wes_palette(n=3, name="Darjeeling1")) +
  scale_y_continuous(limits = c(0,60)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#boxplot by month w outliers
ggplot(boutdoor,aes(x=month,pm2.5, fill = `Sensor`)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  labs(
    y = expression(PM2.5 - (μg/~m^3)),
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "without outliers"
    )) +
  scale_fill_manual(labels = c("Andy","Julie","Lorraine"),values=wes_palette(n=3, name="Darjeeling1")) +
  #scale_y_continuous(limits = c(0,60)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#overall boxplot
ggplot(boutdoor,aes(x=`Sensor`,pm2.5, fill = `Sensor`)) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom = "errorbar") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="yellow") +
  labs(
    y = expression(PM2.5 - (μg/~m^3)),
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "without outliers"
    )) +
  scale_fill_manual(labels = c("Andy","Julie","Lorraine"),values=wes_palette(n=3, name="Darjeeling1")) +
  scale_y_continuous(limits = c(0,30)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
 
#overall boxplot - w/ outliers
ggplot(boutdoor,aes(x=NULL,pm2.5, fill = `Sensor`)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  labs(
    y = expression(PM2.5 - (μg/~m^3)),
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "with outliers"
    )) +
  scale_fill_manual(labels = c("Andy","Julie","Lorraine"),values=wes_palette(n=3, name="Darjeeling1")) +
  #scale_y_continuous(limits = c(0,30)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


#correlation plots
outdoor <- outdoor %>%
  select(
    pm2p5.lorraine,pm2p5.julie,pm2p5,pm10p0.lorraine,pm10p0.julie, pm10p0) %>%
  rename(
    pm2p5.andy = pm2p5,
    pm10p0.andy = pm10p0
    )
chart.Correlation(outdoor)
