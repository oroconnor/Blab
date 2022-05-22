
#Packages needed for function

library(tidyverse)
library(lubridate)


###########
# Test code
testdf <- read_csv("AMNC_QAQ_April_clean.csv") %>%
  select(
    time, pm25
  )



most_recent_time <- max(testdf$time) # calculate most recent time in dataset
twelve_hours_ago <- most_recent_time - hours(12) # calculate 12 hours before that  
testdf <-testdf %>%
  filter(
    time >= twelve_hours_ago
  )

##############
# Function code

#takes the last 12 hours of data and creates 12 hourly concentration averages
hour_munge <- function (
  df # dataframe that contains the past 12 hours of PM observations. First column timestamp. Second column PM2.5
) {
  
    most_recent_time <- max(df$time) # calculate most recent time in dataset
    twelve_hours_ago <- most_recent_time - hours(12) # calculate 12 hours before that
    
    df <- df %>%
      filter(
        time >= twelve_hours_ago
      ) %>%
      mutate(
        time_from_recent = floor(as.numeric(as.duration(most_recent_time-time), "hours"))
      )
    
    #Round values on the edge that are 12 hours down into the "11th hour"
    df$time_from_recent[df$time_from_recent == 12] <- 11  

    hourly_avgs <- df %>%
      group_by(
        time_from_recent
      ) %>%
      summarise(
        pm25 = mean(pm25,na.rm = TRUE)
      )
    
  return(hourly_avgs)
}

nowcast <- function (
  df # dataframe that contains the past 12 hours of PM observations. First column timestamp. Second column PM2.5
) {
  hourly_avgs <- hour_munge(df) #takes the last 12 hours of data and creates 12 hourly concentration averages
  
  range <- max(hourly_avgs$pm25) - min(hourly_avgs$pm25) 
  scaled_rate_of_change <- range / max(hourly_avgs$pm25)
  weight_factor <- 1 - scaled_rate_of_change
  if (weight_factor < .5)
    weight_factor <- .5


  hourly_avgs_weighted <- hourly_avgs %>%
    mutate(
    pm25 = pm25 * (weight_factor ^ time_from_recent),
    weights = (weight_factor ^ time_from_recent)
    )
  
  nowcast_num <- sum(hourly_avgs_weighted$pm25) / sum(hourly_avgs_weighted$weights) 
  
  nowcast_num <-  trunc(nowcast_num*10^2)/10^2 # truncate to 2 decimal places
  
  
  return(nowcast_num)
  
} 
