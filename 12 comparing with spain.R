######compare results with high perFOrming benchmark country in europe
rm(list = ls())

setwd("C:/Users/gleno/OneDrive/Documents/ELLIE UNI/PROJECT")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)
library(gridExtra)
library(HMDHFDplus)
library(DemoDecomp)
library(janitor)
library(paletteer)
myHMDusername <- "insert-user"
myHMDpassword <- "insert-password"
##Spain - 83.1 le at 0 in 2022

####importing lt data to calculate average years lived between 0 and 75
FLT_SP <- readHMDweb(
  CNTRY = "ESP",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)
MLT_SP <- readHMDweb(
  CNTRY = "ESP",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)

FLT_SP <- FLT_SP %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "female")
MLT_SP <- MLT_SP %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "male")

SPAIN <- bind_rows(FLT_SP, MLT_SP) %>% mutate(Country = "spain")

####### 2007 PY FOR FEMALES WAS 72.913 AND FOR MALES WAS 70.578.
####### 2022 PY FOR FEMALES WAS 73.237 AND FOR MALES WAS 71.689.
####### CHANGE FOR FEMALES WAS 0.324 AND FOR MALES WAS 1.111

####### 2007 LE FOR FEMALE WAS 84.16 AND FOR MALES WAS 77.75
####### 2022 LE FOR FEMALES WAS 85.71 AND FOR MALES WAS 80.32
####### CHANGE FOR FEMALES WAS 1.55 AND FOR MALES WAS 2.57

####### FROM THIS IT IS OBVIOUS THAT THE GAINS THE HIGHER PERFORMING COUNTRIES HAVE MADE ARE IN THE AGES OF 75 AND OLDER
####### STILL GAINS MADE IN THE AV YEARS LIVED BETWEEN 0 AND 75 YEARS ARE HIGHER FOR MEN (COMPARED TO IRL THE HIGHEST WITH 1.03)
####### SPANISH FEMALES LIVING LONGER ON AVERAGE BUT THIS HAS NOT INCREASED AS MUCH AS IRELAND


##see if italy is better or worse
FLT_it<- readHMDweb(
  CNTRY = "ITA",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)
MLT_it <- readHMDweb(
  CNTRY = "ITA",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)

FLT_it <- FLT_it %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "female")
MLT_it <- MLT_it %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "male")

ITALY <- bind_rows(FLT_it, MLT_it) %>% mutate(Country = "italy")

####italy actually has the largest value for males in 2019 72.08 - down to 71.93 in 2022
