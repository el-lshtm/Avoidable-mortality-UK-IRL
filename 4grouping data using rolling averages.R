##creating 3 years averages instead of single years


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

#subselecting to 3 year intervals
mort_07av <- mort_UK_IR %>% filter(Year %in% c(2007, 2008, 2009))
mort_20av <- mort_UK_IR %>% filter(Year %in% c(2020, 2021, 2022))

mort_07av <- mort_07av %>% mutate(Country = as.factor(Country))
mort_20av <- mort_20av %>% mutate(Country = as.factor(Country))

rm(mort, part1, part2, part3, part4, part5, part6, countries, mort_UK_IR)
#remove AA and A000 -
mort_07av$delete = as.integer(str_detect(mort_07av$Cause,"A000|AA"))
table(mort_07av$delete) #brings up 24
mort_07av <-  mort_07av[mort_07av$delete == 0,]

#subsets cancer causes from D00 to D49
mort_07av$cancer = as.integer(str_detect(mort_07av$Cause,"D0|D1|D2|D3|D4"))

mort_07av$radical <- substr(mort_07av$Cause, 1,1)
mort_07av$capital = 0


# 	C00-D48	all cancers
mort_07av$capital[mort_07av$radical == "C" | mort_07av$cancer == 1] = 4

#doing the same for 2022
#remove AA and A000 -
mort_20av$delete = as.integer(str_detect(mort_20av$Cause,"A000|AA"))
table(mort_20av$delete) #brings up 24
mort_20av <-  mort_20av[mort_20av$delete == 0,]

#subsets cancer causes from D00 to D49
mort_20av$cancer = as.integer(str_detect(mort_20av$Cause,"D0|D1|D2|D3|D4"))

mort_20av$radical <- substr(mort_20av$Cause, 1,1)
mort_20av$capital = 0


# 	C00-D48	all cancers
mort_20av$capital[mort_20av$radical == "C" | mort_20av$cancer == 1] = 4

#new df of only cancer causes, also drops admin1 and subdiv
#get ride of duplicate death categories IM-deaths(more precise counts of deaths2)
#no unknown age deaths for these years of interest - drop deaths 26. keep deaths1 = total/all ages

canc200709 <- mort_07av %>% filter(capital==4) %>% subset(select = -c(Admin1, SubDiv, Frmat, IM_Frmat, Deaths26, 
                                                                   IM_Deaths1, IM_Deaths2, IM_Deaths3,
                                                                   IM_Deaths4, delete, cancer, 
                                                                   radical, capital, 
                                                                   List, Country_Name))
canc202022 <- mort_20av %>% filter(capital==4) %>% subset(select = -c(Admin1, SubDiv, Frmat, IM_Frmat, Deaths26, 
                                                                   IM_Deaths1, IM_Deaths2, IM_Deaths3,
                                                                   IM_Deaths4, delete, cancer, 
                                                                   radical, capital, 
                                                                   List, Country_Name))

longcanc07av <- canc200709 %>% pivot_longer(cols = starts_with("Deaths"),
                                          names_to = "age_group",
                                          values_to = "death_count")
#C00-C14, C15, C16, C22, C33-C34, C45, C43, C67, C53 - preventable under 75 years
#C18-C21, C50, C54, C55, C62, C73, C81, C91.0, C91.1, D10-D36 = treatable under 75 years
longcanc07av <- longcanc07av %>% mutate(avoidable = case_when(
  age_group %in% c("Deaths21", "Deaths22", "Deaths23", "Deaths24", "Deaths25") ~ "not avoidable",
  startsWith(Cause, "C00") ~ "preventable", #lip (malignant neoplasm of...)
  startsWith(Cause, "C01") ~"preventable", #base of tongue
  startsWith(Cause, "C02") ~"preventable", #other parts of tongue
  startsWith(Cause, "C03") ~"preventable", #gum
  startsWith(Cause, "C04") ~"preventable", #floor of mouth
  startsWith(Cause, "C05") ~"preventable", #palate
  startsWith(Cause, "C06") ~"preventable", #other parts of mouth
  startsWith(Cause, "C07") ~"preventable", #parotid
  startsWith(Cause, "C08") ~"preventable", #other salivary glands
  startsWith(Cause, "C09") ~"preventable", #tonsil
  startsWith(Cause, "C10") ~"preventable", #oropharynx
  startsWith(Cause, "C11") ~"preventable", #nasopharynx
  startsWith(Cause, "C12") ~"preventable", #piriform sinus
  startsWith(Cause, "C13") ~"preventable", #hypopharynx
  startsWith(Cause, "C14") ~"preventable", #other ill defined sites lip/oral cavity/pharynx
  startsWith(Cause, "C15") ~"preventable", #oesophagus
  startsWith(Cause, "C16") ~"preventable", #stomach
  startsWith(Cause, "C22") ~"preventable", #liver
  startsWith(Cause, "C33") ~"preventable", #trachea
  startsWith(Cause, "C34") ~"preventable", #bronchus/lung
  startsWith(Cause, "C45") ~"preventable", #mesothelioma
  startsWith(Cause, "C43") ~"preventable", #melanoma of skin
  startsWith(Cause, "C67") ~"preventable", #bladder
  startsWith(Cause, "C53") ~"50/50", #cervical
  startsWith(Cause, "C18") ~ "treatable", #colon
  startsWith(Cause, "C19") ~ "treatable", #rectosigmoid junction
  startsWith(Cause, "C20") ~ "treatable", #rectum
  startsWith(Cause, "C21") ~ "treatable", #anus/anal canal
  startsWith(Cause, "C50") ~ "treatable", #breast female only
  startsWith(Cause, "C54") ~ "treatable", #corpus uteri
  startsWith(Cause, "C55") ~ "treatable", #uterus part unspecified
  startsWith(Cause, "C62") ~ "treatable", #testis
  startsWith(Cause, "C73") ~ "treatable", #thyroid
  startsWith(Cause, "C81") ~ "treatable", #hodgkin lymphoma
  startsWith(Cause, "C910") ~ "treatable", #acute lymphoblastic leukemia
  startsWith(Cause, "C911") ~ "treatable", #chronic lymphocytic leukemia
  startsWith(Cause, "D1") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D2") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D30") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D31") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D32") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D33") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D34") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D35") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D36") ~ "treatable", #benign neoplasms
  TRUE ~ "not avoidable"
))

##now the same for 2022 df
longcanc20av <- canc202022 %>% pivot_longer(cols = starts_with("Deaths"),
                                          names_to = "age_group",
                                          values_to = "death_count")

longcanc20av <- longcanc20av %>% mutate(avoidable = case_when(
  age_group %in% c("Deaths21", "Deaths22", "Deaths23", "Deaths24", "Deaths25") ~ "not avoidable",
  startsWith(Cause, "C00") ~ "preventable", #lip (malignant neoplasm of...)
  startsWith(Cause, "C01") ~"preventable", #base of tongue
  startsWith(Cause, "C02") ~"preventable", #other parts of tongue
  startsWith(Cause, "C03") ~"preventable", #gum
  startsWith(Cause, "C04") ~"preventable", #flooor of mouth
  startsWith(Cause, "C05") ~"preventable", #palate
  startsWith(Cause, "C06") ~"preventable", #other parts of mouth
  startsWith(Cause, "C07") ~"preventable", #parotid
  startsWith(Cause, "C08") ~"preventable", #other salivary glands
  startsWith(Cause, "C09") ~"preventable", #tonsil
  startsWith(Cause, "C10") ~"preventable", #oropharynx
  startsWith(Cause, "C11") ~"preventable", #nasopharynx
  startsWith(Cause, "C12") ~"preventable", #piriform sinus
  startsWith(Cause, "C13") ~"preventable", #hypopharynx
  startsWith(Cause, "C14") ~"preventable", #other ill defined sites lip/oral cavity/pharynx
  startsWith(Cause, "C15") ~"preventable", #oesophagus
  startsWith(Cause, "C16") ~"preventable", #stomach
  startsWith(Cause, "C22") ~"preventable", #liver
  startsWith(Cause, "C33") ~"preventable", #trachea
  startsWith(Cause, "C34") ~"preventable", #bronchus/lung
  startsWith(Cause, "C45") ~"preventable", #mesothelioma
  startsWith(Cause, "C43") ~"preventable", #melanoma of skin
  startsWith(Cause, "C67") ~"preventable", #bladder
  startsWith(Cause, "C53") ~"50/50", #cervical - !!!also 50% treatable? what to allocate it to
  startsWith(Cause, "C18") ~ "treatable", #colon
  startsWith(Cause, "C19") ~ "treatable", #rectosigmoid junction
  startsWith(Cause, "C20") ~ "treatable", #rectum
  startsWith(Cause, "C21") ~ "treatable", #anus/anal canal
  startsWith(Cause, "C50") ~ "treatable", #breast female only
  startsWith(Cause, "C54") ~ "treatable", #corpus uteri
  startsWith(Cause, "C55") ~ "treatable", #uterus part unspecified
  startsWith(Cause, "C62") ~ "treatable", #testis
  startsWith(Cause, "C73") ~ "treatable", #thyroid
  startsWith(Cause, "C81") ~ "treatable", #hodgkin lymphoma
  startsWith(Cause, "C910") ~ "treatable", #acute lymphoblastic leukemia
  startsWith(Cause, "C911") ~ "treatable", #chronic lymphocytic leukemia
  startsWith(Cause, "D1") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D2") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D30") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D31") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D32") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D33") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D34") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D35") ~ "treatable", #benign neoplasms
  startsWith(Cause, "D36") ~ "treatable", #benign neoplasms
  TRUE ~ "not avoidable"
))


longcanc07av <- longcanc07av %>% mutate(Sex = case_when(
  Sex == 1 ~ "male",
  Sex == 2 ~ "female"
), Country = case_when(
  Country == 4310 ~ "england_wales",
  Country == 4320 ~ "northern_irl",
  Country == 4330 ~ "scotland",
  Country == 4170 ~ "ireland"
), age_group = case_when(
  age_group == "Deaths1" ~ "all ages",
  age_group == "Deaths2"~ "<1",
  age_group %in% c("Deaths3", "Deaths4", "Deaths5", "Deaths6") ~ "1-4",
  age_group == "Deaths7" ~ "5-9",
  age_group == "Deaths8" ~ "10-14",
  age_group == "Deaths9" ~ "15-19",
  age_group == "Deaths10" ~ "20-24",
  age_group == "Deaths11" ~ "25-29",
  age_group == "Deaths12" ~ "30-34",
  age_group == "Deaths13" ~ "35-39",
  age_group == "Deaths14" ~ "40-44",
  age_group == "Deaths15" ~ "45-49",
  age_group == "Deaths16" ~ "50-54",
  age_group == "Deaths17" ~ "55-59",
  age_group == "Deaths18" ~ "60-64",
  age_group == "Deaths19" ~ "65-69",
  age_group == "Deaths20" ~ "70-74",
  age_group == "Deaths21" ~ "75-79",
  age_group == "Deaths22" ~ "80-84",
  age_group %in% c("Deaths23", "Deaths24", "Deaths25") ~ ">85"
)) %>% 
  group_by(Country, Sex, Year, Cause, age_group, avoidable) %>% 
  summarise(death_count = sum(death_count, na.rm = TRUE), .groups = "drop")


#correct order of age groups 
longcanc07av <- longcanc07av %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85", "all ages"
  )))
longcanc07av %>% arrange(age_group)
levels(longcanc07av$age_group)

#looks like its working
#same for 2022
longcanc20av <- longcanc20av %>% mutate(Sex = case_when(
  Sex == 1 ~ "male",
  Sex == 2 ~ "female"
), Country = case_when(
  Country == 4310 ~ "england_wales",
  Country == 4320 ~ "northern_irl",
  Country == 4330 ~ "scotland",
  Country == 4170 ~ "ireland"
), age_group = case_when(
  age_group == "Deaths1" ~ "all ages",
  age_group == "Deaths2"~ "<1",
  age_group %in% c("Deaths3", "Deaths4", "Deaths5", "Deaths6") ~ "1-4",
  age_group == "Deaths7" ~ "5-9",
  age_group == "Deaths8" ~ "10-14",
  age_group == "Deaths9" ~ "15-19",
  age_group == "Deaths10" ~ "20-24",
  age_group == "Deaths11" ~ "25-29",
  age_group == "Deaths12" ~ "30-34",
  age_group == "Deaths13" ~ "35-39",
  age_group == "Deaths14" ~ "40-44",
  age_group == "Deaths15" ~ "45-49",
  age_group == "Deaths16" ~ "50-54",
  age_group == "Deaths17" ~ "55-59",
  age_group == "Deaths18" ~ "60-64",
  age_group == "Deaths19" ~ "65-69",
  age_group == "Deaths20" ~ "70-74",
  age_group == "Deaths21" ~ "75-79",
  age_group == "Deaths22" ~ "80-84",
  age_group %in% c("Deaths23", "Deaths24", "Deaths25") ~ ">85"
)) %>% 
  group_by(Country, Sex, Year, Cause, age_group, avoidable) %>% 
  summarise(death_count = sum(death_count, na.rm = TRUE), .groups = "drop")

#correct order of age groups 
longcanc20av <- longcanc20av %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85", "all ages"
  )))
longcanc20av %>% arrange(age_group)

##now doing the same for all non cancer causes

other200709 <- mort_07av %>%  subset(select = -c(Admin1, SubDiv, Frmat, IM_Frmat, Deaths26, 
                                              IM_Deaths1, IM_Deaths2, IM_Deaths3,
                                              IM_Deaths4, delete, cancer, 
                                              radical, capital, 
                                              List, Country_Name))
other202022 <- mort_20av %>% subset(select = -c(Admin1, SubDiv, Frmat, IM_Frmat, Deaths26, 
                                             IM_Deaths1, IM_Deaths2, IM_Deaths3,
                                             IM_Deaths4, delete, cancer, 
                                             radical, capital, 
                                             List, Country_Name))


longother2007 <- other200709 %>% pivot_longer(cols = starts_with("Deaths"),
                                            names_to = "age_group",
                                            values_to = "death_count")


longother2007 <- longother2007 %>% mutate(avoidable = case_when(
  age_group %in% c("Deaths21", "Deaths22", "Deaths23", "Deaths24", "Deaths25") ~ "not avoidable",
  startsWith(Cause, "A0") ~ "preventable", #intestinal infectious diseases
  startsWith(Cause, "A33") ~"preventable", #tetanus neonatorum
  startsWith(Cause, "A34") ~"preventable", #obstetric tetanus 
  startsWith(Cause, "A35") ~"preventable", #oohter tetanus
  startsWith(Cause, "A36") ~"preventable", #diptheria
  startsWith(Cause, "A37") ~"preventable", #whooping cough
  startsWith(Cause, "A39") ~"preventable", #meningococcal infection
  startsWith(Cause, "A34") ~"preventable", #obstetric tetanus 
  startsWith(Cause, "A403") ~"preventable", #sepsis due to streptococcus pneumonia
  startsWith(Cause, "A413") ~"preventable", #sepsis due to hemophilius influenzae
  startsWith(Cause, "A492") ~"preventable", #hemophilius influenza infection
  startsWith(Cause, "A5") ~"preventable", #stis
  startsWith(Cause, "A60") ~"preventable", #stis
  startsWith(Cause, "A63") ~"preventable", #stis
  startsWith(Cause, "A64") ~"preventable", #stis
  startsWith(Cause, "B01") ~"preventable", #varicella
  startsWith(Cause, "B05") ~"preventable", #measles
  startsWith(Cause, "B06") ~"preventable", #rubella
  startsWith(Cause, "B15") ~"preventable", #acute hepatitis a
  startsWith(Cause, "B16") ~"preventable", #acute hepatitis b
  startsWith(Cause, "B17") ~"preventable", #other acute hepatitis 
  startsWith(Cause, "B18") ~"preventable", #chronic viral hepatitis
  startsWith(Cause, "B19") ~"preventable", #unspecified viral hepatitis
  startsWith(Cause, "B20") ~"preventable", #hiv/aids related infections
  startsWith(Cause, "B21") ~"preventable", #hiv/aids related neoplasms
  startsWith(Cause, "B22") ~"preventable", #hiv/aids related diseases
  startsWith(Cause, "B23") ~"preventable", #hiv/aids related other
  startsWith(Cause, "B24") ~"preventable", #unspecified hiv
  startsWith(Cause, "B50") ~"preventable", #malaria
  startsWith(Cause, "B51") ~"preventable", #malaria
  startsWith(Cause, "B52") ~"preventable", #malaria
  startsWith(Cause, "B53") ~"preventable", #malaria
  startsWith(Cause, "B54") ~"preventable", #malaria
  startsWith(Cause, "G000") ~"preventable", #haemophilius meningitis
  startsWith(Cause, "G001") ~"preventable", #pneumococcal meningitis
  startsWith(Cause, "A15") ~"50/50", #respiratory tb confirmed - !!!all tb 50/50 between preventable and treatable
  startsWith(Cause, "A16") ~"50/50", #respiratory tb not confirmed
  startsWith(Cause, "A17") ~"50/50", #tb of nervous system
  startsWith(Cause, "A18") ~"50/50", #tb of other organs
  startsWith(Cause, "A19") ~"50/50", #miliary tb
  startsWith(Cause, "B90") ~"50/50", #sequalae of tb
  startsWith(Cause, "J65") ~"50/50", #pneumoconiosis assoc with tb
  startsWith(Cause, "A38") ~"treatable", #scarlet fever
  startsWith(Cause, "A40") ~"treatable", #strep sepsis, should exclude a403
  startsWith(Cause, "A41") ~"treatable", #other sepsis, should exclude a413
  startsWith(Cause, "A46") ~"treatable", #cellulitis
  startsWith(Cause, "L03") ~"treatable", #cellulitis
  startsWith(Cause, "A481") ~"treatable", #legionnaires
  startsWith(Cause, "A491") ~"treatable", #streptococcal infections
  startsWith(Cause, "G002") ~"treatable", #other meningitis
  startsWith(Cause, "G003") ~"treatable", #other meningitis
  startsWith(Cause, "G008") ~"treatable", #other meningitis
  startsWith(Cause, "G009") ~"treatable", #other meningitis
  startsWith(Cause, "G03 ") ~"treatable", #other meningitis
  startsWith(Cause, "D50") ~"preventable", #nutritional definciency anaemia
  startsWith(Cause, "D51") ~"preventable", #nutritional definciency anaemia
  startsWith(Cause, "D52") ~"preventable", #nutritional definciency anaemia
  startsWith(Cause, "D53") ~"preventable", #nutritional definciency anaemia
  startsWith(Cause, "E10") ~"50/50", #diabetes type 1 (e10 -e14 50/50)
  startsWith(Cause, "E11") ~"50/50", #diabetes type 2
  startsWith(Cause, "E12") ~"50/50", #other diabetes
  startsWith(Cause, "E13") ~"50/50", #other diabetes
  startsWith(Cause, "E14") ~"50/50", #unspecified diabetes
  startsWith(Cause, "E0") ~"treatable", #thyroid disorders
  startsWith(Cause, "E244") ~"preventable", #alcohol induced pseudo cushings
  startsWith(Cause, "E24") ~"treatable", #cushings
  startsWith(Cause, "E25") ~"treatable", #adrenogenital disorders
  startsWith(Cause, "E27") ~"treatable", #other adrenal gland disorders
  startsWith(Cause, "G40") ~"treatable", #epilepsy
  startsWith(Cause, "G41") ~"treatable", #status epilepticus
  startsWith(Cause, "I71") ~"50/50", #aortic aneurysm/disscetion
  startsWith(Cause, "I1") ~"50/50", #hypertensive disease
  startsWith(Cause, "I20") ~"50/50", #IHD - angina
  startsWith(Cause, "I21") ~"50/50", #IHD - acute myocardia infarct
  startsWith(Cause, "I22") ~"50/50", #IHD - subsequent MI
  startsWith(Cause, "I23") ~"50/50", #IHD - complications
  startsWith(Cause, "I24") ~"50/50", #other IHD
  startsWith(Cause, "I25") ~"50/50", #chronic IHD
  startsWith(Cause, "I70") ~"50/50", #athersclerosis
  startsWith(Cause, "I739") ~"50/50", #other ahterosclerosis
  startsWith(Cause, "I6") ~"50/50", #all cerebrovascular disease (haemorrhage, stroke etc)
  startsWith(Cause, "I0") ~"treatable", #rheumatic/heart and chronic
  startsWith(Cause, "I26") ~"treatable", #pulmonary embolism
  startsWith(Cause, "I80") ~"treatable", #phlebitis
  startsWith(Cause, "I82.9") ~"treatable", #other VTE
  startsWith(Cause, "J09") ~"preventable", #influenza
  startsWith(Cause, "J10") ~"preventable", #influenza
  startsWith(Cause, "J11") ~"preventable", #influenza
  startsWith(Cause, "J13") ~"preventable", #pneumonia due to strep
  startsWith(Cause, "J14") ~"preventable", #pneumonia due to haemophlius flue
  startsWith(Cause, "J40") ~"preventable", #bronchitis
  startsWith(Cause, "J41") ~"preventable", #chronic bronchitis
  startsWith(Cause, "J42") ~"preventable", #unspecified bronchitis
  startsWith(Cause, "J43") ~"preventable", #emphysema
  startsWith(Cause, "J44") ~"preventable", #other copd
  startsWith(Cause, "J6") ~"preventable", #lung disease due to external agents, should exc j65
  startsWith(Cause, "J70") ~"preventable", #other resp conditions due to ext agents
  startsWith(Cause, "J82") ~"preventable", #pulm eosinophilia
  startsWith(Cause, "J92") ~"preventable", #pleural plaque
  startsWith(Cause, "J0") ~"treatable", #upper resp infections
  startsWith(Cause, "J3") ~"treatable", #other upper resp tract infections
  startsWith(Cause, "J12") ~"treatable", #viral pneumonia
  startsWith(Cause, "J15") ~"treatable", #bacterial pneumonia
  startsWith(Cause, "J16") ~"treatable", #other pneumonia
  startsWith(Cause, "J17") ~"treatable", #other pneumonia
  startsWith(Cause, "J18") ~"treatable", #other pneumonia
  startsWith(Cause, "J2") ~"treatable", #acute lrti
  startsWith(Cause, "J45") ~"treatable", #asthma
  startsWith(Cause, "J46") ~"treatable", #asthma
  startsWith(Cause, "J47") ~"treatable", #bronchiecstasis
  startsWith(Cause, "J80") ~"treatable", #adult respi distress syndrome
  startsWith(Cause, "J81") ~"treatable", #pulm oedema
  startsWith(Cause, "J85") ~"treatable", #lung/mediastinum abscess
  startsWith(Cause, "J86") ~"treatable", #pyothorax
  startsWith(Cause, "J90") ~"treatable", #other pleural disorders
  startsWith(Cause, "J93") ~"treatable", #ther pleural disorders
  startsWith(Cause, "J94") ~"treatable", #ther pleural disorders
  startsWith(Cause, "K292") ~"preventable", #alcoholic gastritis
  startsWith(Cause, "I426") ~"preventable", #alcoholic cardiomyopathy
  startsWith(Cause, "F10") ~"preventable", #mental disorder due to alchol
  startsWith(Cause, "G312") ~"preventable", #degen of nervous system due to alcohol
  startsWith(Cause, "G621") ~"preventable", #alcoholic polyneuropathy
  startsWith(Cause, "G721") ~"preventable", #alcoholic myopathy
  startsWith(Cause, "K70") ~"preventable", #alcoholic liver disease
  startsWith(Cause, "K852") ~"preventable", #alcoholic induced pancreatitis
  startsWith(Cause, "K860") ~"preventable", #alcoholic induced chronic panc
  startsWith(Cause, "Q860") ~"preventable", #fetal alcohol syndrome
  startsWith(Cause, "R780") ~"preventable", #alcohol in blood
  startsWith(Cause, "X45") ~"preventable", #accidental poisoning by alcohol
  startsWith(Cause, "X65") ~"preventable", #intentional posioning by alc
  startsWith(Cause, "Y15") ~"preventable", #poisoning by alc uknown intent
  startsWith(Cause, "K73") ~"preventable", #chronis hepatitis
  startsWith(Cause, "K740") ~"preventable", #hepatic fibrosis
  startsWith(Cause, "K741") ~"preventable", #heaptic sclerosis
  startsWith(Cause, "K742") ~"preventable", #hepatic fibrosis
  startsWith(Cause, "K746") ~"preventable", #other cirrhosis
  startsWith(Cause, "F11") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F12") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F13") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F14") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F15") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F16") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F18") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F19") ~"preventable", #disorders due to drugs
  startsWith(Cause, "X40") ~"preventable", #acc poisoning
  startsWith(Cause, "X41") ~"preventable", #acc poisoning
  startsWith(Cause, "X42") ~"preventable", #acc poisoning
  startsWith(Cause, "X43") ~"preventable", #acc poisoning
  startsWith(Cause, "X44") ~"preventable", #acc poisoning
  startsWith(Cause, "X85") ~"preventable", #assault by drugs
  startsWith(Cause, "X60") ~"preventable", #intentional self poisoning
  startsWith(Cause, "X61") ~"preventable", #intentional self poisoning
  startsWith(Cause, "X62") ~"preventable", #intentional self poisoning
  startsWith(Cause, "X63") ~"preventable", #intentional self poisoning
  startsWith(Cause, "X64") ~"preventable", #intentional self poisoning
  startsWith(Cause, "Y10") ~"preventable", #undetermined poisoning
  startsWith(Cause, "Y11") ~"preventable", #undetermined poisoning
  startsWith(Cause, "Y12") ~"preventable", #undetermined poisoning
  startsWith(Cause, "Y13") ~"preventable", #undetermined poisoning
  startsWith(Cause, "Y14") ~"preventable", #undetermined poisoning
  startsWith(Cause, "U071") ~"preventable", #covid
  startsWith(Cause, "U072") ~"preventable", #covid
  startsWith(Cause, "K25") ~"treatable", #gastric ulcer
  startsWith(Cause, "K26") ~"treatable", #duodenal ulcer
  startsWith(Cause, "K27") ~"treatable", #peptic ulcer
  startsWith(Cause, "K28") ~"treatable", #gastrojejunal ulcer
  startsWith(Cause, "K35") ~"treatable", #appendicitis
  startsWith(Cause, "K36") ~"treatable", #other appendicitis
  startsWith(Cause, "K37") ~"treatable", #unspec appendicitis
  startsWith(Cause, "K38") ~"treatable", #other appendix
  startsWith(Cause, "K4") ~"treatable", #hernias
  startsWith(Cause, "K80") ~"treatable", #chollithiasis
  startsWith(Cause, "K81") ~"treatable", #cholecystitis
  startsWith(Cause, "K82") ~"treatable", #gallbladder disease
  startsWith(Cause, "K83") ~"treatable", #biliary tract
  startsWith(Cause, "K850") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K851") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K853") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K858") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K859") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K861") ~"treatable", #other pancreas
  startsWith(Cause, "K862") ~"treatable", #other pancreas
  startsWith(Cause, "K863") ~"treatable", #other pancreas
  startsWith(Cause, "K868") ~"treatable", #other pancreas
  startsWith(Cause, "K869") ~"treatable", #other pancreas
  startsWith(Cause, "N00") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N01") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N02") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N03") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N04") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N05") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N06") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N07") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N13") ~"treatable", #obstructive uropathy
  startsWith(Cause, "N20") ~"treatable", #obstructive uropathy
  startsWith(Cause, "N21") ~"treatable", #obstructive uropathy
  startsWith(Cause, "N35") ~"treatable", #obstructive uropathy
  startsWith(Cause, "N17") ~"treatable", #renal failure
  startsWith(Cause, "N18") ~"treatable", #renal failure
  startsWith(Cause, "N19") ~"treatable", #renal failure
  startsWith(Cause, "N23") ~"treatable", #renal colic
  startsWith(Cause, "N25") ~"treatable", #renal tubular disfunciton
  startsWith(Cause, "N26") ~"treatable", #contracted kidney
  startsWith(Cause, "N27") ~"treatable", #small kidney
  startsWith(Cause, "N341") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N70") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N71") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N72") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N73") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N750") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N751") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N764") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N766") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N40") ~"treatable", #prostatic hyperplasia
  startsWith(Cause, "O") ~"treatable", #pregnancy/childbirth/puerperium
  startsWith(Cause, "P") ~"treatable", #conditions originating in perinatal perios
  startsWith(Cause, "Q00") ~"preventable", #anencephaly
  startsWith(Cause, "Q01") ~"preventable", #encephalocele
  startsWith(Cause, "Q05") ~"preventable", #spina bifida
  startsWith(Cause, "Q2") ~"treatable", #congenital heart defects
  startsWith(Cause, "Y4") ~"treatable", #complicaitons from drugs/medicine
  startsWith(Cause, "Y5") ~"treatable", #complicaitons from drugs/medicine
  startsWith(Cause, "Y6") ~"treatable", #misadventures during care
  startsWith(Cause, "Y83") ~"treatable", #misadventures during care
  startsWith(Cause, "Y84") ~"treatable", #misadventures during care
  startsWith(Cause, "Y7") ~"treatable", #adverse incidents from medical devices
  startsWith(Cause, "Y80") ~"treatable", #adverse incidents from medical devices
  startsWith(Cause, "Y81") ~"treatable", #adverse incidents from medical devices
  startsWith(Cause, "Y82") ~"treatable", #adverse incidents from medical devices
  startsWith(Cause, "V") ~"preventable", #transport incidents
  startsWith(Cause, "W") ~"preventable", #accidental injuries (falls etc)
  startsWith(Cause, "X0") ~"preventable", #exposure to smoke/fire
  startsWith(Cause, "X1") ~"preventable", #exposure to heat
  startsWith(Cause, "X2") ~"preventable", #exposure to venomous animals
  startsWith(Cause, "X3") ~"preventable", #exposure to nature
  startsWith(Cause, "X4") ~"preventable", #poisonings
  startsWith(Cause, "X5") ~"preventable", #overexrtion, privation, unspecified
  startsWith(Cause, "X6") ~"preventable", #intentional poisoning
  startsWith(Cause, "X7") ~"preventable", #self harm
  startsWith(Cause, "X8") ~"preventable", #self harm and assault
  startsWith(Cause, "X9") ~"preventable", #assault
  startsWith(Cause, "Y0") ~"preventable", #assault
  startsWith(Cause, "Y16") ~"preventable", #solvent poisoning
  startsWith(Cause, "Y17") ~"preventable", #carbon monoxide poisoning
  startsWith(Cause, "Y18") ~"preventable", #pesticide poisoning
  startsWith(Cause, "Y19") ~"preventable", #unspecified poisoning
  startsWith(Cause, "Y2") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y30") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y31") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y32") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y33") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y34") ~"preventable", #accidents with undetermined intent
  TRUE ~ "not avoidable"
))


#tidying up the labels  
longother2007 <- longother2007 %>% mutate(Sex = case_when(
  Sex == 1 ~ "male",
  Sex == 2 ~ "female"
), Country = case_when(
  Country == 4310 ~ "england_wales",
  Country == 4320 ~ "northern_irl",
  Country == 4330 ~ "scotland",
  Country == 4170 ~ "ireland"
), age_group = case_when(
  age_group == "Deaths1" ~ "all ages",
  age_group == "Deaths2"~ "<1",
  age_group %in% c("Deaths3", "Deaths4", "Deaths5", "Deaths6") ~ "1-4",
  age_group == "Deaths7" ~ "5-9",
  age_group == "Deaths8" ~ "10-14",
  age_group == "Deaths9" ~ "15-19",
  age_group == "Deaths10" ~ "20-24",
  age_group == "Deaths11" ~ "25-29",
  age_group == "Deaths12" ~ "30-34",
  age_group == "Deaths13" ~ "35-39",
  age_group == "Deaths14" ~ "40-44",
  age_group == "Deaths15" ~ "45-49",
  age_group == "Deaths16" ~ "50-54",
  age_group == "Deaths17" ~ "55-59",
  age_group == "Deaths18" ~ "60-64",
  age_group == "Deaths19" ~ "65-69",
  age_group == "Deaths20" ~ "70-74",
  age_group == "Deaths21" ~ "75-79",
  age_group == "Deaths22" ~ "80-84",
  age_group %in% c("Deaths23", "Deaths24", "Deaths25") ~ ">85"
))  %>% 
  group_by(Country, Sex, Year, Cause, age_group, avoidable) %>% 
  summarise(death_count = sum(death_count, na.rm = TRUE), .groups = "drop")

#correct order of age groups 
longother2007 <- longother2007 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85", "all ages"
  )))
longother2007 %>% arrange(age_group)





#now the same with 2022 other causes

longother2020 <- other202022 %>% pivot_longer(cols = starts_with("Deaths"),
                                            names_to = "age_group",
                                            values_to = "death_count")


longother2020 <- longother2020 %>% mutate(avoidable = case_when(
  age_group %in% c("Deaths21", "Deaths22", "Deaths23", "Deaths24", "Deaths25") ~ "not avoidable",
  startsWith(Cause, "A0") ~ "preventable", #intestinal infectious diseases
  startsWith(Cause, "A33") ~"preventable", #tetanus neonatorum
  startsWith(Cause, "A34") ~"preventable", #obstetric tetanus 
  startsWith(Cause, "A35") ~"preventable", #oohter tetanus
  startsWith(Cause, "A36") ~"preventable", #diptheria
  startsWith(Cause, "A37") ~"preventable", #whooping cough
  startsWith(Cause, "A39") ~"preventable", #meningococcal infection
  startsWith(Cause, "A34") ~"preventable", #obstetric tetanus 
  startsWith(Cause, "A403") ~"preventable", #sepsis due to streptococcus pneumonia
  startsWith(Cause, "A413") ~"preventable", #sepsis due to hemophilius influenzae
  startsWith(Cause, "A492") ~"preventable", #hemophilius influenza infection
  startsWith(Cause, "A5") ~"preventable", #stis
  startsWith(Cause, "A60") ~"preventable", #stis
  startsWith(Cause, "A63") ~"preventable", #stis
  startsWith(Cause, "A64") ~"preventable", #stis
  startsWith(Cause, "B01") ~"preventable", #varicella
  startsWith(Cause, "B05") ~"preventable", #measles
  startsWith(Cause, "B06") ~"preventable", #rubella
  startsWith(Cause, "B15") ~"preventable", #acute hepatitis a
  startsWith(Cause, "B16") ~"preventable", #acute hepatitis b
  startsWith(Cause, "B17") ~"preventable", #other acute hepatitis 
  startsWith(Cause, "B18") ~"preventable", #chronic viral hepatitis
  startsWith(Cause, "B19") ~"preventable", #unspecified viral hepatitis
  startsWith(Cause, "B20") ~"preventable", #hiv/aids related infections
  startsWith(Cause, "B21") ~"preventable", #hiv/aids related neoplasms
  startsWith(Cause, "B22") ~"preventable", #hiv/aids related diseases
  startsWith(Cause, "B23") ~"preventable", #hiv/aids related other
  startsWith(Cause, "B24") ~"preventable", #unspecified hiv
  startsWith(Cause, "B50") ~"preventable", #malaria
  startsWith(Cause, "B51") ~"preventable", #malaria
  startsWith(Cause, "B52") ~"preventable", #malaria
  startsWith(Cause, "B53") ~"preventable", #malaria
  startsWith(Cause, "B54") ~"preventable", #malaria
  startsWith(Cause, "G000") ~"preventable", #haemophilius meningitis
  startsWith(Cause, "G001") ~"preventable", #pneumococcal meningitis
  startsWith(Cause, "A15") ~"50/50", #respiratory tb confirmed - !!!all tb 50/50 between preventable and treatable
  startsWith(Cause, "A16") ~"50/50", #respiratory tb not confirmed
  startsWith(Cause, "A17") ~"50/50", #tb of nervous system
  startsWith(Cause, "A18") ~"50/50", #tb of other organs
  startsWith(Cause, "A19") ~"50/50", #miliary tb
  startsWith(Cause, "B90") ~"50/50", #sequalae of tb
  startsWith(Cause, "J65") ~"50/50", #pneumoconiosis assoc with tb
  startsWith(Cause, "A38") ~"treatable", #scarlet fever
  startsWith(Cause, "A40") ~"treatable", #strep sepsis, should exclude a403
  startsWith(Cause, "A41") ~"treatable", #other sepsis, should exclude a413
  startsWith(Cause, "A46") ~"treatable", #cellulitis
  startsWith(Cause, "L03") ~"treatable", #cellulitis
  startsWith(Cause, "A481") ~"treatable", #legionnaires
  startsWith(Cause, "A491") ~"treatable", #streptococcal infections
  startsWith(Cause, "G002") ~"treatable", #other meningitis
  startsWith(Cause, "G003") ~"treatable", #other meningitis
  startsWith(Cause, "G008") ~"treatable", #other meningitis
  startsWith(Cause, "G009") ~"treatable", #other meningitis
  startsWith(Cause, "G03 ") ~"treatable", #other meningitis
  startsWith(Cause, "D50") ~"preventable", #nutritional definciency anaemia
  startsWith(Cause, "D51") ~"preventable", #nutritional definciency anaemia
  startsWith(Cause, "D52") ~"preventable", #nutritional definciency anaemia
  startsWith(Cause, "D53") ~"preventable", #nutritional definciency anaemia
  startsWith(Cause, "E10") ~"50/50", #diabetes type 1 (e10 -e14 50/50)
  startsWith(Cause, "E11") ~"50/50", #diabetes type 2
  startsWith(Cause, "E12") ~"50/50", #other diabetes
  startsWith(Cause, "E13") ~"50/50", #other diabetes
  startsWith(Cause, "E14") ~"50/50", #unspecified diabetes
  startsWith(Cause, "E0") ~"treatable", #thyroid disorders
  startsWith(Cause, "E244") ~"preventable", #alcohol induced pseudo cushings
  startsWith(Cause, "E24") ~"treatable", #cushings
  startsWith(Cause, "E25") ~"treatable", #adrenogenital disorders
  startsWith(Cause, "E27") ~"treatable", #other adrenal gland disorders
  startsWith(Cause, "G40") ~"treatable", #epilepsy
  startsWith(Cause, "G41") ~"treatable", #status epilepticus
  startsWith(Cause, "I71") ~"50/50", #aortic aneurysm/disscetion
  startsWith(Cause, "I1") ~"50/50", #hypertensive disease
  startsWith(Cause, "I20") ~"50/50", #IHD - angina
  startsWith(Cause, "I21") ~"50/50", #IHD - acute myocardia infarct
  startsWith(Cause, "I22") ~"50/50", #IHD - subsequent MI
  startsWith(Cause, "I23") ~"50/50", #IHD - complications
  startsWith(Cause, "I24") ~"50/50", #other IHD
  startsWith(Cause, "I25") ~"50/50", #chronic IHD
  startsWith(Cause, "I70") ~"50/50", #athersclerosis
  startsWith(Cause, "I739") ~"50/50", #other ahterosclerosis
  startsWith(Cause, "I6") ~"50/50", #all cerebrovascular disease (haemorrhage, stroke etc)
  startsWith(Cause, "I0") ~"treatable", #rheumatic/heart and chronic
  startsWith(Cause, "I26") ~"treatable", #pulmonary embolism
  startsWith(Cause, "I80") ~"treatable", #phlebitis
  startsWith(Cause, "I82.9") ~"treatable", #other VTE
  startsWith(Cause, "J09") ~"preventable", #influenza
  startsWith(Cause, "J10") ~"preventable", #influenza
  startsWith(Cause, "J11") ~"preventable", #influenza
  startsWith(Cause, "J13") ~"preventable", #pneumonia due to strep
  startsWith(Cause, "J14") ~"preventable", #pneumonia due to haemophlius flue
  startsWith(Cause, "J40") ~"preventable", #bronchitis
  startsWith(Cause, "J41") ~"preventable", #chronic bronchitis
  startsWith(Cause, "J42") ~"preventable", #unspecified bronchitis
  startsWith(Cause, "J43") ~"preventable", #emphysema
  startsWith(Cause, "J44") ~"preventable", #other copd
  startsWith(Cause, "J6") ~"preventable", #lung disease due to external agents, should exc j65
  startsWith(Cause, "J70") ~"preventable", #other resp conditions due to ext agents
  startsWith(Cause, "J82") ~"preventable", #pulm eosinophilia
  startsWith(Cause, "J92") ~"preventable", #pleural plaque
  startsWith(Cause, "J0") ~"treatable", #upper resp infections
  startsWith(Cause, "J3") ~"treatable", #other upper resp tract infections
  startsWith(Cause, "J12") ~"treatable", #viral pneumonia
  startsWith(Cause, "J15") ~"treatable", #bacterial pneumonia
  startsWith(Cause, "J16") ~"treatable", #other pneumonia
  startsWith(Cause, "J17") ~"treatable", #other pneumonia
  startsWith(Cause, "J18") ~"treatable", #other pneumonia
  startsWith(Cause, "J2") ~"treatable", #acute lrti
  startsWith(Cause, "J45") ~"treatable", #asthma
  startsWith(Cause, "J46") ~"treatable", #asthma
  startsWith(Cause, "J47") ~"treatable", #bronchiecstasis
  startsWith(Cause, "J80") ~"treatable", #adult respi distress syndrome
  startsWith(Cause, "J81") ~"treatable", #pulm oedema
  startsWith(Cause, "J85") ~"treatable", #lung/mediastinum abscess
  startsWith(Cause, "J86") ~"treatable", #pyothorax
  startsWith(Cause, "J90") ~"treatable", #other pleural disorders
  startsWith(Cause, "J93") ~"treatable", #ther pleural disorders
  startsWith(Cause, "J94") ~"treatable", #ther pleural disorders
  startsWith(Cause, "K292") ~"preventable", #alcoholic gastritis
  startsWith(Cause, "I426") ~"preventable", #alcoholic cardiomyopathy
  startsWith(Cause, "F10") ~"preventable", #mental disorder due to alchol
  startsWith(Cause, "G312") ~"preventable", #degen of nervous system due to alcohol
  startsWith(Cause, "G621") ~"preventable", #alcoholic polyneuropathy
  startsWith(Cause, "G721") ~"preventable", #alcoholic myopathy
  startsWith(Cause, "K70") ~"preventable", #alcoholic liver disease
  startsWith(Cause, "K852") ~"preventable", #alcoholic induced pancreatitis
  startsWith(Cause, "K860") ~"preventable", #alcoholic induced chronic panc
  startsWith(Cause, "Q860") ~"preventable", #fetal alcohol syndrome
  startsWith(Cause, "R780") ~"preventable", #alcohol in blood
  startsWith(Cause, "X45") ~"preventable", #accidental poisoning by alcohol
  startsWith(Cause, "X65") ~"preventable", #intentional posioning by alc
  startsWith(Cause, "Y15") ~"preventable", #poisoning by alc uknown intent
  startsWith(Cause, "K73") ~"preventable", #chronis hepatitis
  startsWith(Cause, "K740") ~"preventable", #hepatic fibrosis
  startsWith(Cause, "K741") ~"preventable", #heaptic sclerosis
  startsWith(Cause, "K742") ~"preventable", #hepatic fibrosis
  startsWith(Cause, "K746") ~"preventable", #other cirrhosis
  startsWith(Cause, "F11") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F12") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F13") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F14") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F15") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F16") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F18") ~"preventable", #disorders due to drugs
  startsWith(Cause, "F19") ~"preventable", #disorders due to drugs
  startsWith(Cause, "X40") ~"preventable", #acc poisoning
  startsWith(Cause, "X41") ~"preventable", #acc poisoning
  startsWith(Cause, "X42") ~"preventable", #acc poisoning
  startsWith(Cause, "X43") ~"preventable", #acc poisoning
  startsWith(Cause, "X44") ~"preventable", #acc poisoning
  startsWith(Cause, "X85") ~"preventable", #assault by drugs
  startsWith(Cause, "X60") ~"preventable", #intentional self poisoning
  startsWith(Cause, "X61") ~"preventable", #intentional self poisoning
  startsWith(Cause, "X62") ~"preventable", #intentional self poisoning
  startsWith(Cause, "X63") ~"preventable", #intentional self poisoning
  startsWith(Cause, "X64") ~"preventable", #intentional self poisoning
  startsWith(Cause, "Y10") ~"preventable", #undetermined poisoning
  startsWith(Cause, "Y11") ~"preventable", #undetermined poisoning
  startsWith(Cause, "Y12") ~"preventable", #undetermined poisoning
  startsWith(Cause, "Y13") ~"preventable", #undetermined poisoning
  startsWith(Cause, "Y14") ~"preventable", #undetermined poisoning
  startsWith(Cause, "U071") ~"preventable", #covid
  startsWith(Cause, "U072") ~"preventable", #covid
  startsWith(Cause, "K25") ~"treatable", #gastric ulcer
  startsWith(Cause, "K26") ~"treatable", #duodenal ulcer
  startsWith(Cause, "K27") ~"treatable", #peptic ulcer
  startsWith(Cause, "K28") ~"treatable", #gastrojejunal ulcer
  startsWith(Cause, "K35") ~"treatable", #appendicitis
  startsWith(Cause, "K36") ~"treatable", #other appendicitis
  startsWith(Cause, "K37") ~"treatable", #unspec appendicitis
  startsWith(Cause, "K38") ~"treatable", #other appendix
  startsWith(Cause, "K4") ~"treatable", #hernias
  startsWith(Cause, "K80") ~"treatable", #chollithiasis
  startsWith(Cause, "K81") ~"treatable", #cholecystitis
  startsWith(Cause, "K82") ~"treatable", #gallbladder disease
  startsWith(Cause, "K83") ~"treatable", #biliary tract
  startsWith(Cause, "K850") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K851") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K853") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K858") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K859") ~"treatable", #acute pancreatitis
  startsWith(Cause, "K861") ~"treatable", #other pancreas
  startsWith(Cause, "K862") ~"treatable", #other pancreas
  startsWith(Cause, "K863") ~"treatable", #other pancreas
  startsWith(Cause, "K868") ~"treatable", #other pancreas
  startsWith(Cause, "K869") ~"treatable", #other pancreas
  startsWith(Cause, "N00") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N01") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N02") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N03") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N04") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N05") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N06") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N07") ~"treatable", #nephritis/nephrosis
  startsWith(Cause, "N13") ~"treatable", #obstructive uropathy
  startsWith(Cause, "N20") ~"treatable", #obstructive uropathy
  startsWith(Cause, "N21") ~"treatable", #obstructive uropathy
  startsWith(Cause, "N35") ~"treatable", #obstructive uropathy
  startsWith(Cause, "N17") ~"treatable", #renal failure
  startsWith(Cause, "N18") ~"treatable", #renal failure
  startsWith(Cause, "N19") ~"treatable", #renal failure
  startsWith(Cause, "N23") ~"treatable", #renal colic
  startsWith(Cause, "N25") ~"treatable", #renal tubular disfunciton
  startsWith(Cause, "N26") ~"treatable", #contracted kidney
  startsWith(Cause, "N27") ~"treatable", #small kidney
  startsWith(Cause, "N341") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N70") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N71") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N72") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N73") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N750") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N751") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N764") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N766") ~"treatable", #inflammatory diseas of genitourinary
  startsWith(Cause, "N40") ~"treatable", #prostatic hyperplasia
  startsWith(Cause, "O") ~"treatable", #pregnancy/childbirth/puerperium
  startsWith(Cause, "P") ~"treatable", #conditions originating in perinatal perios
  startsWith(Cause, "Q00") ~"preventable", #anencephaly
  startsWith(Cause, "Q01") ~"preventable", #encephalocele
  startsWith(Cause, "Q05") ~"preventable", #spina bifida
  startsWith(Cause, "Q2") ~"treatable", #congenital heart defects
  startsWith(Cause, "Y4") ~"treatable", #complicaitons from drugs/medicine
  startsWith(Cause, "Y5") ~"treatable", #complicaitons from drugs/medicine
  startsWith(Cause, "Y6") ~"treatable", #misadventures during care
  startsWith(Cause, "Y83") ~"treatable", #misadventures during care
  startsWith(Cause, "Y84") ~"treatable", #misadventures during care
  startsWith(Cause, "Y7") ~"treatable", #adverse incidents from medical devices
  startsWith(Cause, "Y80") ~"treatable", #adverse incidents from medical devices
  startsWith(Cause, "Y81") ~"treatable", #adverse incidents from medical devices
  startsWith(Cause, "Y82") ~"treatable", #adverse incidents from medical devices
  startsWith(Cause, "V") ~"preventable", #transport incidents
  startsWith(Cause, "W") ~"preventable", #accidental injuries (falls etc)
  startsWith(Cause, "X0") ~"preventable", #exposure to smoke/fire
  startsWith(Cause, "X1") ~"preventable", #exposure to heat
  startsWith(Cause, "X2") ~"preventable", #exposure to venomous animals
  startsWith(Cause, "X3") ~"preventable", #exposure to nature
  startsWith(Cause, "X4") ~"preventable", #poisonings
  startsWith(Cause, "X5") ~"preventable", #overexrtion, privation, unspecified
  startsWith(Cause, "X6") ~"preventable", #intentional poisoning
  startsWith(Cause, "X7") ~"preventable", #self harm
  startsWith(Cause, "X8") ~"preventable", #self harm and assault
  startsWith(Cause, "X9") ~"preventable", #assault
  startsWith(Cause, "Y0") ~"preventable", #assault
  startsWith(Cause, "Y16") ~"preventable", #solvent poisoning
  startsWith(Cause, "Y17") ~"preventable", #carbon monoxide poisoning
  startsWith(Cause, "Y18") ~"preventable", #pesticide poisoning
  startsWith(Cause, "Y19") ~"preventable", #unspecified poisoning
  startsWith(Cause, "Y2") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y30") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y31") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y32") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y33") ~"preventable", #accidents with undetermined intent
  startsWith(Cause, "Y34") ~"preventable", #accidents with undetermined intent
  TRUE ~ "not avoidable"
))

#tidying up the labels  
longother2020 <- longother2020 %>% mutate(Sex = case_when(
  Sex == 1 ~ "male",
  Sex == 2 ~ "female"
), Country = case_when(
  Country == 4310 ~ "england_wales",
  Country == 4320 ~ "northern_irl",
  Country == 4330 ~ "scotland",
  Country == 4170 ~ "ireland"
), age_group = case_when(
  age_group == "Deaths1" ~ "all ages",
  age_group == "Deaths2"~ "<1",
  age_group %in% c("Deaths3", "Deaths4", "Deaths5", "Deaths6") ~ "1-4",
  age_group == "Deaths7" ~ "5-9",
  age_group == "Deaths8" ~ "10-14",
  age_group == "Deaths9" ~ "15-19",
  age_group == "Deaths10" ~ "20-24",
  age_group == "Deaths11" ~ "25-29",
  age_group == "Deaths12" ~ "30-34",
  age_group == "Deaths13" ~ "35-39",
  age_group == "Deaths14" ~ "40-44",
  age_group == "Deaths15" ~ "45-49",
  age_group == "Deaths16" ~ "50-54",
  age_group == "Deaths17" ~ "55-59",
  age_group == "Deaths18" ~ "60-64",
  age_group == "Deaths19" ~ "65-69",
  age_group == "Deaths20" ~ "70-74",
  age_group == "Deaths21" ~ "75-79",
  age_group == "Deaths22" ~ "80-84",
  age_group %in% c("Deaths23", "Deaths24", "Deaths25") ~ ">85"
))  %>% 
  group_by(Country, Sex, Year, Cause, age_group, avoidable) %>% 
  summarise(death_count = sum(death_count, na.rm = TRUE), .groups = "drop")

#correct order of age groups 
longother2020 <- longother2020 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85", "all ages"
  )))
longother2020 %>% arrange(age_group)


ca_prop0709 <- longcanc07av %>%
  filter(age_group != "all ages") %>%
  group_by(Country, Sex, Year, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, Year, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()
#reordering age groups
ca_prop0709 <- ca_prop0709 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85"
  )))

ca_prop2022 <- longcanc20av %>%
  filter(age_group != "all ages") %>%
  group_by(Country, Sex, Year, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, Year, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()

ca_prop2022 <- ca_prop2022 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85"
  )))

otherprop0709 <- longother2007 %>%
  filter(age_group != "all ages") %>%
  group_by(Country, Sex, Year, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, Year, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()

otherprop0709 <- otherprop0709 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85"
  )))

otherprop2022 <- longother2020 %>%
  filter(age_group != "all ages") %>%
  group_by(Country, Sex, Year, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, Year, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  group_by(Year) %>% 
  mutate(
    av_deaths = mean(deaths)
  )
  ungroup()

otherprop2022 <- otherprop2022 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85"
  )))
head(otherprop2022)
##how to get 3 year average
otherprop2022_av <- otherprop2022 %>% group_by(Country, Sex, age_group, avoidable) %>% 
  summarise(
    av_deaths = mean(deaths),
    av_total_deaths = mean(total_deaths),
    av_proportion = mean(proportion),
    .groups = "drop")
otherprop0709_av <- otherprop0709 %>% group_by(Country, Sex, age_group, avoidable) %>% 
  summarise(
    av_deaths = mean(deaths),
    av_total_deaths = mean(total_deaths),
    av_proportion = mean(proportion),
    .groups = "drop")
caprop2022_av <- ca_prop2022 %>% group_by(Country, Sex, age_group, avoidable) %>% 
  summarise(
    av_deaths = mean(deaths),
    av_total_deaths = mean(total_deaths),
    av_proportion = mean(proportion),
    .groups = "drop")
caprop0709_av <- ca_prop0709 %>% group_by(Country, Sex, age_group, avoidable) %>% 
  summarise(
    av_deaths = mean(deaths),
    av_total_deaths = mean(total_deaths),
    av_proportion = mean(proportion),
    .groups = "drop")


rm(mort_07av, mort_20av, other200709, other2007av, other202022, other2020av, longcanc07av, longcanc20av, longother2007, longother2020)
##now i have averages - can combines cancer and other non cancer causes for each time period

combi_0709 <- left_join(otherprop0709_av, caprop0709_av, 
                      join_by(Country==Country, Sex == Sex, age_group==age_group, avoidable==avoidable),
                      suffix = c(".o", ".c"))

##all 50/50 cancers for males have NA values - sub these to 0 then update proportions/totals
combi_0709 <- combi_0709 %>% replace(is.na(.), 0)

combi_0709 <- combi_0709 %>% group_by(Country, Sex, age_group) %>% mutate(
  av_total_deaths.c = sum(av_deaths.c),
  av_proportion.c = (av_deaths.c / av_total_deaths.c)
)

combi_0709 <- combi_0709 %>% group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths.b = (av_total_deaths.o + av_total_deaths.c),
    proportion.b = (av_deaths.o + av_deaths.c) / total_deaths.b
  ) %>%
  ungroup()


##doing the same for 2022
combi_2022 <- left_join(otherprop2022_av, caprop2022_av, 
                        join_by(Country==Country, Sex == Sex, age_group==age_group, avoidable==avoidable),
                        suffix = c(".o", ".c"))

##all 50/50 cancers for males have NA values - sub these to 0 then update proportions/totals
combi_2022 <- combi_2022 %>% replace(is.na(.), 0)

combi_2022 <- combi_2022 %>% group_by(Country, Sex, age_group) %>% mutate(
  av_total_deaths.c = sum(av_deaths.c),
  av_proportion.c = (av_deaths.c / av_total_deaths.c)
)

combi_2022 <- combi_2022 %>% group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths.b = (av_total_deaths.o + av_total_deaths.c),
    proportion.b = (av_deaths.o + av_deaths.c) / total_deaths.b
  ) %>%
  ungroup()

combi_0709 %>%
  ggplot(aes(x = age_group, y = total_deaths.b, fill = avoidable)) +
  facet_wrap(~Country) +
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = c(
    "preventable"   = "seagreen",
    "treatable"     = "palegreen",
    "50/50"         = "goldenrod", 
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Total deaths",
    fill = "Avoidability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##make long in order to separate cancer and non cancer


long_0709 <- combi_0709 %>%
  pivot_longer(
    cols = c(av_deaths.c, av_deaths.o),
    names_to = "death_type",
    values_to = "deaths"
  ) %>%
  mutate(death_type = recode(death_type,
                             "av_deaths.c" = "cancer",
                             "av_deaths.o" = "other"
  ))

long_0709 <- long_0709 %>%
  mutate(fill_group = paste(avoidable, death_type, sep = " - "))

long_0709 <- long_0709 %>%
  mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

long_0709 %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>% 
  ggplot(aes(x = age_group, y = deaths, fill = fill_group)) +
  facet_wrap(~ Country) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "green",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "grey",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
  labs(
    x = "Age group",
    y = "Total deaths",
    fill = "Avoidability",
    title = "Avoidable deaths by cancer and other causes: 2007 to 2009 average"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##same for average 2020-2022 period

long_2022 <- combi_2022 %>%
  pivot_longer(
    cols = c(av_deaths.c, av_deaths.o),
    names_to = "death_type",
    values_to = "deaths"
  ) %>%
  mutate(death_type = recode(death_type,
                             "av_deaths.c" = "cancer",
                             "av_deaths.o" = "other"
  ))

long_2022 <- long_2022 %>%
  mutate(fill_group = paste(avoidable, death_type, sep = " - "))

long_2022 <- long_2022 %>%
  mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

long_2022 %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>% 
  ggplot(aes(x = age_group, y = deaths, fill = fill_group)) +
  facet_wrap(~ Country) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "green",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "grey",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
  labs(
    x = "Age group",
    y = "Total deaths",
    fill = "Avoidability",
    title = "Avoidable deaths by cancer and other causes: 2020 to 2022 average"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##changing female deaths to negative values in order to make pop pyramids
long_0709_sex <- long_0709 %>%
  mutate(deaths_pyramid = ifelse(Sex == "female", -deaths, deaths))

long_2022_sex <- long_2022 %>%
  mutate(deaths_pyramid = ifelse(Sex == "female", -deaths, deaths))

long_2022_sex %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>%
  ggplot(aes(x = age_group, y = deaths_pyramid, fill = fill_group)) +
  facet_wrap(~ Country) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "green",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "grey",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
  scale_y_continuous(labels = abs) +  # show positive numbers on both sides
  coord_flip() +                       # flip so age groups are on y axis
  labs(
    x = "Age group",
    y = "Total deaths",
    fill = "Avoidability",
    caption = "← Female | Male →",
    title = "Deaths within the UK and Ireland by avoidability: 2020-2022"
  ) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.25)) 

long_0709_sex %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>%
  ggplot(aes(x = age_group, y = deaths_pyramid, fill = fill_group)) +
  facet_wrap(~ Country) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "green",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "grey",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
  scale_y_continuous(labels = abs) +  # show positive numbers on both sides
  coord_flip() +                       # flip so age groups are on y axis
  labs(
    x = "Age group",
    y = "Total deaths",
    fill = "Avoidability",
    caption = "← Female | Male →",
    title = "Deaths within the UK and Ireland by avoidability: 2007-2009"
  ) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.25)) 

##upon visual comparison with figures only for 2007 and 2022 there isn't much difference
##perhaps larger no.s of preventable- other causes, as this is what covid19 deaths are classified as
##females in EW in 70-74 no of preventable other deaths - 4590.333 - for males - 6613.667
sum(long_2022$deaths) #887069
sum(long_0709$deaths) #768231 - quite a lot fewer 
##females in EW in 70-74 no of preventable other deaths - 2040- for males - 2657

##how have these deaths changed as a proportion of the deaths


longprop2022 <- long_2022_sex %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    proportion.b = deaths_pyramid / total_deaths.b
  ) %>%
  ungroup()

longprop0709 <- long_0709_sex %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    proportion.b = deaths_pyramid / total_deaths.b
  ) %>%
  ungroup()

longprop2022 %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>%
  ggplot(aes(x = age_group, y = proportion.b, fill = fill_group)) +
  facet_wrap(~ Country) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "green",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "grey",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
  scale_y_continuous(labels = abs) +  # show positive numbers on both sides
  coord_flip() +                       # flip so age groups are on y axis
  labs(
    x = "Age group",
    y = "Proportion of total deaths",
    fill = "Avoidability",
    caption = "← Female | Male →",
    title = "2020-2022 - Proportion of deaths within the UK and Ireland by avoidability, Cancer and Other causes"
  ) +
  theme_classic()  


longprop0709 %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>%
  ggplot(aes(x = age_group, y = proportion.b, fill = fill_group)) +
  facet_wrap(~ Country) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "green",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "grey",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
  scale_y_continuous(labels = abs) +  # show positive numbers on both sides
  coord_flip() +                       # flip so age groups are on y axis
  labs(
    x = "Age group",
    y = "Proportion of total deaths",
    fill = "Avoidability",
    caption = "← Female | Male →",
    title = "2007-2009 - Proportion of deaths within the UK and Ireland by avoidability, Cancer and Other causes"
  ) +
  theme_classic()  

##save these for easy access
export(long_0709_sex, file = "data/avoidable_deaths_07_09average.csv")
export(long_2022_sex, file = "data/avoidable_deaths_20_22average.csv")