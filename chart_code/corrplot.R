library(tidyverse)
library(feather)
library(corrplot)

#read in the tibble prepared by airmasterk.R script
airmaster <- read_feather("airmasterk.feather")
airmaster <- airmaster %>%
  select(-c(X1,ObsID,"Unit ID",Alarms,Time,weekday,))
airmaster <- drop_na(airmaster)
corrplot(cor(airmaster), method = "ellipse")