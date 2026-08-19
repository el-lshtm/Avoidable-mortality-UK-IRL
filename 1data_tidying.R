#INITIAL DATA EXPLORATION 


setwd("C:/Users/gleno/OneDrive/Documents/ELLIE UNI/PROJECT")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)
countries <- read.csv("data/country_codes")

part1 <- read.csv("data/Morticd10_part1")
part2 <- read.csv("data/Morticd10_part2")
part3 <- read.csv("data/Morticd10_part3")
part4 <- read.csv("data/Morticd10_part4")
part5 <- read.csv("data/Morticd10_part5")
part6 <- read.csv("data/Morticd10_part6")

#binding all years together
mort <- rbind(part1, part2, part3, part4, part5, part6)


#merge country codes to country name in subsetted df
mort <- merge(mort, countries, by.x = "Country", by.y = "country")


#rename 'name' to 'Country_Name'
names(mort)[names(mort) == "name"] <- "Country_Name"

#subselecting to countries of interest
#4310 (England and Wales) 4320 (NOrthern Ireland) 4330 (Scotland) 4170 (Ireland)
mort_UK_IR <- mort %>% filter(Country %in% c(4310, 4320, 4330, 4170))
mort_IR <- mort %>% filter(Country == c(4170))




#only have data for scotland in 2000? have data for all of UK in 2001, do not have data for Ireland until 2007 
mort_2000 <- mort_UK_IR %>% filter(Year == 2000)
mort_2001 <- mort_UK_IR %>% filter(Year == 2001)

mort_2007 <- mort_UK_IR %>% filter(Year == 2007)
mort_2022 <- mort_UK_IR %>% filter(Year == 2022)

#most recent year we have data for all 4 countries is 2022
unique(mort_2007$Country)


deaths_2007 <- mort_2007 %>% group_by(Country) %>% summarise(Totaldeaths = sum(Deaths1))
deaths_2022 <- mort_2022 %>% group_by(Country) %>% summarise(Totaldeaths = sum(Deaths1))

deaths_comb <- deaths_2007 %>% inner_join(deaths_2022, by = "Country", suffix = c("_2007", "_2022"))

ggplot(deaths_2022, aes(x = factor(Country), y = Totaldeaths)) + geom_col() + labs(title = "Total deaths in 2022")
ggplot(deaths_2007, aes(x = factor(Country), y = Totaldeaths)) + geom_col() + labs(title = "Total deaths in 2007")


#drop admin1 and subdiv?

export(mort_2007, file = "data/mortalitycounts_2007.csv")
export(mort_2022, file = "data/mortalitycounts_2022.csv")

##need data for 2019 in order to do sensitivity analysis of covid effect
mort_2019 <- mort_UK_IR %>% filter(Year == 2019)
deaths_2019 <- mort_2019 %>% group_by(Country) %>% summarise(Totaldeaths = sum(Deaths1))
export(mort_2019, file = "data/mortalitycounts_2019.csv")