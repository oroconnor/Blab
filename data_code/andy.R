#load in MetOne data and Egg data from Andi Murphy Center and
#combine the two datasets


library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)
library(RCurl)
library(PerformanceAnalytics)
library(Metrics)
library(psych)
library(wesanderson)
library(mgcv)

#read in the tibble prepared by airmasterk.R script
airmasterk <- read_feather("airmasterk.feather")

metmasterk <- airmasterk %>% 
  select(
    Time,Temp,RH,pm2.5,pm10
  ) %>%
  rename(
    time = Time,
    temp_met = Temp,
    rh_met = RH,
    pm2.5_met = pm2.5,
    pm10_met = pm10
    )



#Gets the data from Egg API. TAKES MORE THAN AN HOUR TO RUN. 
#Load in already prepared tibble below instead unless you need to update.
csv_data = ""
start_date = as.POSIXct("2020/05/04")
end_date = as.POSIXct("2021/01/15")
while (start_date < end_date) {
  start_date_as_str = format(start_date, format="%Y-%m-%dT%H:%M:00Z")
  next_date = start_date + (24 * 60 * 60)
  next_date_as_str = format(next_date, format="%Y-%m-%dT%H:%M:00Z")
  egg_url = paste("https://airqualityegg.com/api/v2/messages/device/egg008044b967880111?apiKey=JE5TCQQ-288MGVF-JE4AQ8T-HPQS0HR&format=csv&start-date=", start_date_as_str, "&end-date=", next_date_as_str,"&resolution=PT01M", sep = "")    
  csv_data_for_period = getURL(egg_url)
  csv_data = paste(csv_data, csv_data_for_period, "\n")
  start_date = next_date
}

eggmasterk <- read_csv(csv_data)

#save as a tibble for easy loading
write_feather(eggmasterk,"eggmasterk.feather")

#read in the tibble prepared by above
eggmasterk <- read_feather("eggmasterk.feather")

eggmasterk <- eggmasterk %>%
  select(
    `time`,`co2[ppm]`,`o3[ppb]`,`pm10p0[ug/m^3]`,`pm2p5[ug/m^3]`,`temperature[degC]`,`humidity[percent]`
  ) %>%
  rename(
    pm10_egg = `pm10p0[ug/m^3]`,
    pm2.5_egg = `pm2p5[ug/m^3]`,
    co2_egg = `co2[ppm]`,
    o3_egg =`o3[ppb]`,
    temp_egg = `temperature[degC]`,
    rh_egg = `humidity[percent]`
  ) 

eggmasterk$pm10_egg <- as.numeric(eggmasterk$pm10_egg)
eggmasterk$pm2.5_egg <- as.numeric(eggmasterk$pm2.5_egg)
eggmasterk$co2_egg <- as.numeric(eggmasterk$co2_egg)
eggmasterk$o3_egg <- as.numeric(eggmasterk$o3_egg)
eggmasterk$temp_egg <- as.numeric(eggmasterk$temp_egg)
eggmasterk$rh_egg <- as.numeric(eggmasterk$rh_egg)

eggmasterk<-mutate(eggmasterk, time= parse_datetime(time))

#joining metmasterk and eggmasterk
comparek <- inner_join(metmasterk,eggmasterk,by = "time") %>%
 select(-c("time")) %>%
  na.omit()

#pairwise plots and correlation
chart.Correlation(comparek)

cor(comparek)

#two sensors over time, but limited scale
ggplot(comparek, aes(time,y)) +
  geom_point(aes(y = pm2.5_met), size= .3, color = "red", alpha =.4) +
  geom_point(aes(y = pm2.5_egg), size= .3, color = "blue", alpha =.4) +
  theme_classic() +
  scale_y_continuous(limits = c(0,50))

#hexbin
ggplot(comparek, aes(pm2.5_met,pm2.5_egg)) +
  geom_hex(bins=80) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() 

#melted for legend
ggplot(gather(comparek,`Sensor`, sensor,c(pm2.5_met,pm2.5_egg))
       ,aes(time,sensor, color = `Sensor`)) +
  geom_point(size = .3, alpha = .4)

#condensing to hourly
hcomparek <- comparek %>%
  mutate(hour = hour(time), date = as_date(time))
hcomparek <- hcomparek %>%
  group_by(date,hour) %>% #group by the date and hour columns
  summarise(hpm2.5_egg = mean(pm2.5_egg),
            hpm2.5_met = mean(pm2.5_met)
             )

hcomparek <- hcomparek %>%
  mutate(YMDH = ymd_h(paste(date,hour)))%>%
  na.omit()

#melted for legend - Scale limited
ggplot(gather(hcomparek,`Sensor`, sensor,c(hpm2.5_met,hpm2.5_egg)),
       aes(YMDH,sensor, color = `Sensor`)) +
  geom_point(size = .3, alpha = .4) +
  geom_smooth() +
  labs(
    y = expression(Mass - (μg/~m^3)),
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 Hourly Averages"
    )) +
  theme_classic() +
  scale_y_continuous() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling1"),labels = c("Egg","MetOne"))

#condensing to daily
dcomparek <- comparek %>%
  mutate(date = as_date(time))
dcomparek <- dcomparek %>%
  group_by(date) %>% #group by the date and hour columns
  summarise(dpm2.5_egg = mean(pm2.5_egg,na.rm = TRUE),
            dpm2.5_met = mean(pm2.5_met,na.rm = TRUE)
  )

dcomparek <- dcomparek %>%
  mutate(YMD = ymd(date)) %>%
  na.omit()


#melted for legend - Scale limited
ggplot(gather(dcomparek,`Sensor`, sensor,c(dpm2.5_met,dpm2.5_egg)),
       aes(YMD,sensor, color = `Sensor`)) +
  geom_point(size = .3, alpha = .4) +
  geom_smooth() +
  labs(
    y = expression(Mass - (μg/~m^3)),
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 Daily Averages"
    )) +
  theme_classic() +
  scale_y_continuous() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling1"),labels = c("Egg","MetOne"))


cov(dcomparek$dpm2.5_egg,dcomparek$dpm2.5_met,use="complete.obs")
cor(dcomparek$dpm2.5_egg,dcomparek$dpm2.5_met,use="complete.obs")

#Root Mean Squared Error, calling Metone Predicted
result = rmse(dcomparek$dpm2.5_met,dcomparek$dpm2.5_egg)
print(result)

#summary stats of whole dataset
describe(comparek)

summary(comparek)


#boxplots
comparek <- inner_join(metmasterk,eggmasterk,by = "time") %>%
  na.omit() %>%
  mutate(
    month = month(time, label=TRUE)
        )
bcomparek <- gather(comparek,`Sensor`, pm2.5,c(pm2.5_met,pm2.5_egg))

ggplot(bcomparek,aes(x=month,pm2.5, fill = `Sensor`)) +
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
  #scale_y_continuous(limits = c(0,60)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling1"),labels = c("Egg","MetOne"))


#traffic below


tcomparek <- comparek %>%
  mutate(
    month = month(time, label=TRUE),
    dayofweek = wday(time),
    hour = hour(time),
  ) %>%
  filter(
    dayofweek > 1,
    dayofweek < 7,
  ) %>%
  group_by(month,hour) %>% #group by the date and hour columns
  summarise(hpm2.5_egg = mean(pm2.5_egg),
            hpm2.5_met = mean(pm2.5_met),
            hco2_egg = mean(co2_egg),    
            ho3_egg =  mean(o3_egg),
  )


#egg facet hourly
ggplot(data =  tcomparek,aes(x = hour,y = hpm2.5_egg) ) +
  geom_point(aes(y = hpm2.5_egg), color = "red", size= 1) +
  geom_line(aes(y = hpm2.5_egg)) +
  #scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "PM2.5",
    x = "Hour",
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 by hour - Weekdays - Egg"
    )) +
  facet_wrap(~month, ncol = 3, scales = "free") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#MetOne facet hourly
ggplot(data =  tcomparek,aes(x = hour,y = hpm2.5_met) ) +
  geom_point(aes(y = hpm2.5_met), color = "red", size= 1) +
  geom_line(aes(y = hpm2.5_met)) +
  #scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "PM2.5",
    x = "Hour",
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 by hour - Weekdays - MetOne"
    )) +
  facet_wrap(~month, ncol = 3, scales = "free") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#egg Co2 facet hourly
ggplot(data =  tcomparek,aes(x = hour,y = hco2_egg) ) +
  geom_point(aes(y = hco2_egg), color = "red", size= 1) +
  geom_line(aes(y = hco2_egg)) +
  #scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "PM2.5",
    x = "Hour",
    title = paste(
      "2020 CO2 - Kingston"
    ),
    subtitle = paste(
      "CO2 by hour - Weekdays - Egg"
    )) +
  facet_wrap(~month, ncol = 3, scales = "free") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#egg O3 facet hourly
ggplot(data =  tcomparek,aes(x = hour,y = ho3_egg) ) +
  geom_point(aes(y = ho3_egg), color = "red", size= 1) +
  geom_line(aes(y = ho3_egg)) +
  #scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "PM2.5",
    x = "Hour",
    title = paste(
      "2020 O3 - Kingston"
    ),
    subtitle = paste(
      "O3 by hour - Weekdays - Egg"
    )) +
  facet_wrap(~month, ncol = 3, scales = "free") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#just looking at hourly look at weekdays
hcomparek <- comparek %>%
  mutate(
    dayofweek = wday(time),
    hour = hour(time),
  ) %>%
  filter(
    dayofweek > 1,
    dayofweek < 7,
  ) %>%
  group_by(hour) %>% 
  summarise(hpm2.5_egg = mean(pm2.5_egg),
            hpm2.5_met = mean(pm2.5_met),
            hco2_egg = mean(co2_egg),    
            ho3_egg =  mean(o3_egg),
  )

#overall hourly PM2.5 = MetOne
ggplot(data =  hcomparek,aes(x = hour,y = hpm2.5_met) ) +
  geom_point(aes(y = hpm2.5_met), color = "red", size= 1) +
  geom_line(aes(y = hpm2.5_met)) +
#scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "PM2.5",
    x = "Hour",
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 by hour - Weekday - MetOne"
    )) +
theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


#overall hourly PM2.5 - Egg
ggplot(data =  hcomparek,aes(x = hour,y = hpm2.5_egg) ) +
  geom_point(aes(y = hpm2.5_egg), color = "red", size= 1) +
  geom_line(aes(y = hpm2.5_egg)) +
  #scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "PM2.5",
    x = "Hour",
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 by hour - Weekday - Egg"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#overall hourly CO2 - Egg
ggplot(data =  hcomparek,aes(x = hour,y = hco2_egg) ) +
  geom_point(aes(y = hco2_egg), color = "red", size= 1) +
  geom_line(aes(y = hco2_egg)) +
  #scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "CO2",
    x = "Hour",
    title = paste(
      "2020 CO2 - Kingston"
    ),
    subtitle = paste(
      "CO2 by hour - Weekdays - Egg"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#overall hourly O3 - Egg
ggplot(data =  hcomparek,aes(x = hour,y = ho3_egg) ) +
  geom_point(aes(y = ho3_egg), color = "red", size= 1) +
  geom_line(aes(y = ho3_egg)) +
  #scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "O3",
    x = "Hour",
    title = paste(
      "2020 O3 - Kingston"
    ),
    subtitle = paste(
      "O3 by hour - Weekdays - Egg"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

###linear modeling

###lm w weather too
#read in the tibble prepared by weatherdata.R script
fulldf <- read_feather("fulldf.feather")

fullmet <- fulldf %>% 
  select(
    Time,Temp,RH,pm2.5,pm10,avg_wind_sonic_mph, precip_max_hr
  ) %>%
  rename(
    time = Time,
    temp_met = Temp,
    rh_met = RH,
    pm2.5_met = pm2.5,
    pm10_met = pm10
  )

fcomparek <- left_join(fullmet,eggmasterk,by = "time") %>%
 # select(-c("time")) %>%
  na.omit()

#condensing to hourly
fhcomparek <- fcomparek %>%
  mutate(hour = hour(time), date = as_date(time))
fhcomparek <- fhcomparek %>%
  group_by(date,hour) %>% #group by the date and hour columns
  summarise(hpm2.5_egg = mean(pm2.5_egg),
            hpm2.5_met = mean(pm2.5_met),
            temp_egg = mean(temp_egg),
            rh_egg = mean(rh_egg),
            avg_wind_sonic_mph = mean(avg_wind_sonic_mph), 
            precip_max_hr = mean(precip_max_hr)
  )

fhcomparek <- fhcomparek %>%
  mutate(YMDH = ymd_h(paste(date,hour)))%>%
  na.omit()

#splitting into testing and training
dt = sort(sample(nrow(fhcomparek), nrow(fhcomparek)*.7))
train <- fhcomparek[dt,]
test <- fhcomparek[-dt,]

fit <- lm(hpm2.5_met~hpm2.5_egg,data=train)
train$fitted <- fitted(fit)
train$resid <- resid(fit)


#more predictive variables
fit <- lm(hpm2.5_met~hpm2.5_egg + rh_egg + temp_egg + avg_wind_sonic_mph,data=train)
train$fitted <- fitted(fit)

#
fit <- lm(hpm2.5_met~hpm2.5_egg + rh_egg + temp_egg + temp_egg:hpm2.5_egg + avg_wind_sonic_mph,data=train)

#gam
fit <- gam(hpm2.5_met~s(hpm2.5_egg) + s(rh_egg) + s(temp_egg) + s(avg_wind_sonic_mph), data=train)
train$fitted <- fitted(fit)
train$resid <- resid(fit)

#still figuring out: evaluating predicted model
p = predict(fit, test)
cor(p,test$hpm2.5_met)
plot(p,test$hpm2.5_met)


#fhcomparek, now with LM fitted values
#melted for legend - Scale limited
ggplot(gather(train,`Sensor`, sensor,c(hpm2.5_met,hpm2.5_egg,fitted)),
       aes(YMDH,sensor, color = `Sensor`)) +
  geom_point(size = .3, alpha = .4) +
  geom_smooth() +
  labs(
    y = expression(Mass - (μg/~m^3)),
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5"
    )) +
  theme_classic() +
  scale_y_continuous(limits = c(0,40)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=wes_palette(n=3, name="Darjeeling1"),labels = c("MetOne","Egg","Fitted Values"))

#Scatterplot w Met and Egg + RH 
ggplot(data =  fhcomparek,aes(x = hpm2.5_egg,y = hpm2.5_met, color = rh_egg) ) +
  geom_point() +
  geom_smooth(se=FALSE) +
  #scale_y_continuous(limits = c(2,5)) +
  scale_x_continuous(limits = c(0,65)) +
  labs(
    y = "hpm2.5_met",
    x = "hpm2.5_egg",
    title = paste(
      "2020 PM2.5 - Kingston"
    ),
    subtitle = paste(
      "Egg compared to MetOne"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))+
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)


###lm with no weather
fit <- lm(hpm2.5_met~hpm2.5_egg,data=hcomparek)
hcomparek$fitted <- fitted(fit)


#Scatterplot w Met and Egg
ggplot(data =  hcomparek,aes(x = hpm2.5_egg,y = hpm2.5_met) ) +
  geom_hex(bins=60) +
  geom_smooth(method="lm", se=FALSE) +
  #scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "hpm2.5_met",
    x = "hpm2.5_egg",
    title = paste(
      "2020 PM2.5 - Kingston"
    ),
    subtitle = paste(
      "Egg compared to MetOne"
    )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


#hourly, now with LM fitted values
#melted for legend - Scale limited
ggplot(gather(hcomparek,`Sensor`, sensor,c(hpm2.5_met,hpm2.5_egg,fitted)),
       aes(YMDH,sensor, color = `Sensor`)) +
  geom_point(size = .3, alpha = .4) +
  geom_smooth() +
  labs(
    y = expression(Mass - (μg/~m^3)),
    x = NULL,
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 Hourly Averages"
    )) +
  theme_classic() +
  scale_y_continuous(limits = c(0,40)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=wes_palette(n=3, name="Darjeeling1"),labels = c("MetOne","Egg","Fitted Values"))
