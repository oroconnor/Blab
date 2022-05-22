library(tidyverse)

library(feather)

lb_in <- read_feather("LB_In_C_clean.feather")
lb_out <- read_feather("LB_Out_C_clean.feather")
vl_in <- read_feather("VL_In_C_clean.feather")
vl_out <- read_feather("VL_Out_C_clean.feather")