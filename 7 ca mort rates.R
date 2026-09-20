##making cause specific mortality rates
##using hmd mx

rm(list = ls())

setwd("C:/Users/gleno/OneDrive/Documents/ELLIE UNI/PROJECT")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)
library(gridExtra)
library(HMDHFDplus)
data2007 <- import(file = "data/avoidable_deaths_07.csv")

data2022 <- import(file = "data/avoidable_deaths_22.csv")

##give the more sensible age group values - so it matches the hmd
data2007 <- data2007 %>% mutate(age_group = 
                                  recode(age_group, "<1" = "0", "1-4" = "1", "5-9" = "5", "10-14" = "10", "15-19" = "15", "20-24" = "20", "25-29" = "25",
                                         "30-34" = "30", "35-39" = "35", "40-44" = "40", "45-49" = "45", "50-54" = "50", "55-59" = "55", "60-64" = "60",
                                         "65-69" = "65", "70-74" = "70", "75-79" = "75", "80-84" = "80", ">85" = "85")) 
data2022 <- data2022 %>% mutate(age_group = 
                                  recode(age_group, "<1" = "0", "1-4" = "1", "5-9" = "5", "10-14" = "10", "15-19" = "15", "20-24" = "20", "25-29" = "25",
                                         "30-34" = "30", "35-39" = "35", "40-44" = "40", "45-49" = "45", "50-54" = "50", "55-59" = "55", "60-64" = "60",
                                         "65-69" = "65", "70-74" = "70", "75-79" = "75", "80-84" = "80", ">85" = "85"))


myHMDusername <- "insert-user"
myHMDpassword <- "insert-password"


##making country labels
country_labs <- c("england_wales" = "England & Wales", "ireland" = "Republic of Ireland",
                  "northern_irl" = "Northern Ireland", "scotland" = "Scotland")


FLT_ENG_WA <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"))
MLT_ENG_WA <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"))

FLT_ENG_WA <- FLT_ENG_WA %>% filter(Year %in% c("2007", "2022"), !Age %in% c("75", "80", "85", "90", "95", "100", "105", "110"))
MLT_ENG_WA <- MLT_ENG_WA %>% filter(Year %in% c("2007", "2022"), !Age %in% c("75", "80", "85", "90", "95", "100", "105", "110"))

EW_data07f <- data2007 %>% filter(Country=="england_wales", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
EW_data22f <- data2022 %>% filter(Country=="england_wales", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))
EW_f <- bind_rows(EW_data07f, EW_data22f)
EW_data07M <- data2007 %>% filter(Country=="england_wales", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
EW_data22M <- data2022 %>% filter(Country=="england_wales", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))

EW_M <- bind_rows(EW_data07M, EW_data22M) %>% left_join(MLT_ENG_WA, join_by(Year==Year, age_group==Age)) %>% 
  mutate(ca.mx = mx*(total_deaths.c/total_deaths.b))
EW_f <- EW_f %>% left_join(FLT_ENG_WA, join_by(Year==Year, age_group==Age)) %>% 
  mutate(ca.mx = mx*(total_deaths.c/total_deaths.b))
EW_mx_both <- bind_rows(EW_f, EW_M)

##doing the same for other countries
FLT_NIR <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"))
MLT_NIR <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"))

FLT_NIR <- FLT_NIR %>% filter(Year %in% c("2007", "2022"), !Age %in% c("75", "80", "85", "90", "95", "100", "105", "110"))
MLT_NIR <- MLT_NIR %>% filter(Year %in% c("2007", "2022"), !Age %in% c("75", "80", "85", "90", "95", "100", "105", "110"))

NIR_data07f <- data2007 %>% filter(Country=="northern_irl", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
NIR_data22f <- data2022 %>% filter(Country=="northern_irl", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))
NIR_f <- bind_rows(NIR_data07f, NIR_data22f)
NIR_data07M <- data2007 %>% filter(Country=="northern_irl", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
NIR_data22M <- data2022 %>% filter(Country=="northern_irl", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))

NIR_M <- bind_rows(NIR_data07M, NIR_data22M) %>% left_join(MLT_NIR, join_by(Year==Year, age_group==Age)) %>% 
  mutate(ca.mx = mx*(total_deaths.c/total_deaths.b))
NIR_f <- NIR_f %>% left_join(FLT_NIR, join_by(Year==Year, age_group==Age)) %>% 
  mutate(ca.mx = mx*(total_deaths.c/total_deaths.b))
NIR_mx_both <- bind_rows(NIR_f, NIR_M)


##and ireland
FLT_IR <- readHMDweb(
  CNTRY = "IRL",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"))
MLT_IR <- readHMDweb(
  CNTRY = "IRL",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"))

FLT_IR <- FLT_IR %>% filter(Year %in% c("2007", "2022"), !Age %in% c("75", "80", "85", "90", "95", "100", "105", "110"))
MLT_IR <- MLT_IR %>% filter(Year %in% c("2007", "2022"), !Age %in% c("75", "80", "85", "90", "95", "100", "105", "110"))

IR_data07f <- data2007 %>% filter(Country=="ireland", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
IR_data22f <- data2022 %>% filter(Country=="ireland", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))
IR_f <- bind_rows(IR_data07f, IR_data22f)
IR_data07M <- data2007 %>% filter(Country=="ireland", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
IR_data22M <- data2022 %>% filter(Country=="ireland", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))

IR_M <- bind_rows(IR_data07M, IR_data22M) %>% left_join(MLT_IR, join_by(Year==Year, age_group==Age)) %>% 
  mutate(ca.mx = mx*(total_deaths.c/total_deaths.b))
IR_f <- IR_f %>% left_join(FLT_IR, join_by(Year==Year, age_group==Age)) %>% 
  mutate(ca.mx = mx*(total_deaths.c/total_deaths.b))
IR_mx_both <- bind_rows(IR_f, IR_M)

##and sco
FLT_SCO <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"))
MLT_SCO <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"))

FLT_SCO <- FLT_SCO %>% filter(Year %in% c("2007", "2022"), !Age %in% c("75", "80", "85", "90", "95", "100", "105", "110"))
MLT_SCO <- MLT_SCO %>% filter(Year %in% c("2007", "2022"), !Age %in% c("75", "80", "85", "90", "95", "100", "105", "110"))

SCO_data07f <- data2007 %>% filter(Country=="scotland", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
SCO_data22f <- data2022 %>% filter(Country=="scotland", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))
SCO_f <- bind_rows(SCO_data07f, SCO_data22f)
SCO_data07M <- data2007 %>% filter(Country=="scotland", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
SCO_data22M <- data2022 %>% filter(Country=="scotland", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))

SCO_M <- bind_rows(SCO_data07M, SCO_data22M) %>% left_join(MLT_SCO, join_by(Year==Year, age_group==Age)) %>% 
  mutate(ca.mx = mx*(total_deaths.c/total_deaths.b))
SCO_f <- SCO_f %>% left_join(FLT_SCO, join_by(Year==Year, age_group==Age)) %>% 
  mutate(ca.mx = mx*(total_deaths.c/total_deaths.b))
SCO_mx_both <- bind_rows(SCO_f, SCO_M) 

mx_all <- bind_rows(SCO_mx_both, NIR_mx_both, EW_mx_both, IR_mx_both) %>%
  subset(select = c("Country", "Year", "Sex", "age_group", "total_deaths.o", "total_deaths.c", "total_deaths.b", "mx", "ca.mx"))

mx_all %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male") %>% 
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(Sex, Year))) +
  geom_line(colour = "skyblue3", size = 0.6) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "cancer mortality rate, log scale",
    title = "Age-specific cancer mortality rates for males up to 75 years: 2007 to 2022",
    linetype = "Year"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8)) +
  scale_x_discrete(labels = c(
    "0"  = "0–1",
    "1"  = "1–4",
    "5" = "5-9",
    "10" = "10-14",
    "15" = "15-19",
    "20" = "20-24",
    "25" = "25-29",
    "30" = "30-34",
    "35" = "35-39",
    "40" = "40-44",
    "45" = "45-49",
    "50" = "50-54",
    "55" = "55-59",
    "60" = "60-64",
    "65" = "65-69",
    "70" = "70-74"
  )) +
  scale_y_log10()

mx_all %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(Sex, Year))) +
  geom_line(colour = "violetred", size = 0.6) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "cancer mortality rate, log scale",
    title = "Age-specific cancer mortality rates for females up to 75 years: 2007 to 2022",
    linetype = "Year"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8)) +
  scale_x_discrete(labels = c(
    "0"  = "0–1",   
    "1"  = "1–4",
    "5" = "5-9",
    "10" = "10-14",
    "15" = "15-19",
    "20" = "20-24",
    "25" = "25-29",
    "30" = "30-34",
    "35" = "35-39",
    "40" = "40-44",
    "45" = "45-49",
    "50" = "50-54",
    "55" = "55-59",
    "60" = "60-64",
    "65" = "65-69",
    "70" = "70-74"
  )) +
  scale_y_log10() 

##also useful to have 30 to 75
mx_all %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male", !age_group %in% c("0", "1", "5", "10", "15", "20", "25")) %>% 
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(Sex, Year))) +
  geom_line(colour = "skyblue3", size = 0.6) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "cancer mortality rate, log scale",
    title = "Age-specific cancer mortality rates for males 30 to 75 years: 2007 to 2022",
    linetype = "Year"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8)) +
  scale_x_discrete(labels = c(
    "30" = "30-34",
    "35" = "35-39",
    "40" = "40-44",
    "45" = "45-49",
    "50" = "50-54",
    "55" = "55-59",
    "60" = "60-64",
    "65" = "65-69",
    "70" = "70-74"
  )) +
  scale_y_log10()

mx_all %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female",!age_group %in% c("0", "1", "5", "10", "15", "20", "25")) %>% 
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(Sex, Year))) +
  geom_line(colour = "violetred", size = 0.6) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "cancer mortality rate, log scale",
    title = "Age-specific cancer mortality rates for females 30 to 75 years: 2007 to 2022",
    linetype = "Year"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8)) +
  scale_x_discrete(labels = c(
    "30" = "30-34",
    "35" = "35-39",
    "40" = "40-44",
    "45" = "45-49",
    "50" = "50-54",
    "55" = "55-59",
    "60" = "60-64",
    "65" = "65-69",
    "70" = "70-74"
  )) +
  scale_y_log10() 

export(mx_all, file = "data/specmortrates.csv")
##########################################



##make totals of avoidable ca, not just total ca - in order to be able to make avoidable ca mort rates
data2007_avoidable <- data2007 %>% filter(fill_group %in% c("50/50 - cancer", "treatable - cancer", "preventable - cancer")) %>% 
  group_by(Country, Sex, age_group) %>% mutate(deaths_av_ca = sum(deaths)/total_deaths.b)
data2022_avoidable <- data2022 %>% filter(fill_group %in% c("50/50 - cancer", "treatable - cancer", "preventable - cancer")) %>% 
  group_by(Country, Sex, age_group) %>% mutate(deaths_av_ca = sum(deaths)/total_deaths.b)

##separate into f and m for diff years then join onto hmd mx
EW_avoid07f <- data2007_avoidable %>% filter(Country=="england_wales", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
EW_avoid22f <- data2022_avoidable %>% filter(Country=="england_wales", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))
EW_avoidf <- bind_rows(EW_avoid07f, EW_avoid22f)
EW_avoid07m <- data2007_avoidable %>% filter(Country=="england_wales", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
EW_avoid22m <- data2022_avoidable %>% filter(Country=="england_wales", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))

EW_avoidm <- bind_rows(EW_avoid07m, EW_avoid22m) %>% left_join(MLT_ENG_WA, join_by(Year==Year, age_group==Age)) %>% 
  mutate(avca.mx = mx*deaths_av_ca)
EW_avoidf <- EW_avoidf %>% left_join(FLT_ENG_WA, join_by(Year==Year, age_group==Age)) %>% 
  mutate(avca.mx = mx*deaths_av_ca)
EW_avcamx_both <- bind_rows(EW_avoidf, EW_avoidm)

##now for other countries - irl

IRL_avoid07f <- data2007_avoidable %>% filter(Country=="ireland", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
IRL_avoid22f <- data2022_avoidable %>% filter(Country=="ireland", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))
IRL_avoidf <- bind_rows(IRL_avoid07f, IRL_avoid22f)
IRL_avoid07m <- data2007_avoidable %>% filter(Country=="ireland", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
IRL_avoid22m <- data2022_avoidable %>% filter(Country=="ireland", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))

IRL_avoidm <- bind_rows(IRL_avoid07m, IRL_avoid22m) %>% left_join(MLT_IR, join_by(Year==Year, age_group==Age)) %>% 
  mutate(avca.mx = mx*deaths_av_ca)
IRL_avoidf <- IRL_avoidf %>% left_join(FLT_IR, join_by(Year==Year, age_group==Age)) %>% 
  mutate(avca.mx = mx*deaths_av_ca)
IRL_avcamx_both <- bind_rows(IRL_avoidf, IRL_avoidm)

#nir
NIR_avoid07f <- data2007_avoidable %>% filter(Country=="northern_irl", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
NIR_avoid22f <- data2022_avoidable %>% filter(Country=="northern_irl", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))
NIR_avoidf <- bind_rows(NIR_avoid07f, NIR_avoid22f)
NIR_avoid07m <- data2007_avoidable %>% filter(Country=="northern_irl", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
NIR_avoid22m <- data2022_avoidable %>% filter(Country=="northern_irl", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))

NIR_avoidm <- bind_rows(NIR_avoid07m, NIR_avoid22m) %>% left_join(MLT_NIR, join_by(Year==Year, age_group==Age)) %>% 
  mutate(avca.mx = mx*deaths_av_ca)
NIR_avoidf <- NIR_avoidf %>% left_join(FLT_NIR, join_by(Year==Year, age_group==Age)) %>% 
  mutate(avca.mx = mx*deaths_av_ca)
NIR_avcamx_both <- bind_rows(NIR_avoidf, NIR_avoidm)

#sco
SCO_avoid07f <- data2007_avoidable %>% filter(Country=="scotland", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
SCO_avoid22f <- data2022_avoidable %>% filter(Country=="scotland", Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))
SCO_avoidf <- bind_rows(SCO_avoid07f, SCO_avoid22f)
SCO_avoid07m <- data2007_avoidable %>% filter(Country=="scotland", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group))%>% mutate(Year = "2007") %>% 
  mutate(Year = as.numeric(Year))
SCO_avoid22m <- data2022_avoidable %>% filter(Country=="scotland", Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  distinct(Country, Sex, age_group, total_deaths.o, total_deaths.c, total_deaths.b, deaths_av_ca) %>% 
  mutate(age_group = as.numeric(age_group)) %>% mutate(Year = "2022") %>% 
  mutate(Year = as.numeric(Year))

SCO_avoidm <- bind_rows(SCO_avoid07m, SCO_avoid22m) %>% left_join(MLT_SCO, join_by(Year==Year, age_group==Age)) %>% 
  mutate(avca.mx = mx*deaths_av_ca)
SCO_avoidf <- SCO_avoidf %>% left_join(FLT_SCO, join_by(Year==Year, age_group==Age)) %>% 
  mutate(avca.mx = mx*deaths_av_ca)
SCO_avcamx_both <- bind_rows(SCO_avoidf,SCO_avoidm)

avca_mx_all <- bind_rows(SCO_avcamx_both, NIR_avcamx_both, EW_avcamx_both, IRL_avcamx_both) %>%
  subset(select = c("Country", "Year", "Sex", "age_group", "total_deaths.o", "total_deaths.c", "total_deaths.b", "mx", "avca.mx"))

##now to show graphically
##even smaller numbers - defintely need to limit to 30 years and upwards

avca_mx_all %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female",!age_group %in% c("0", "1", "5", "10", "15", "20", "25")) %>% 
  ggplot(aes(x = age_group, y = avca.mx, 
             linetype = factor(Year),
             group = interaction(Sex, Year))) +
  geom_line(colour = "violetred", size = 0.6) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Avoidable cancer mortality rate, log scale",
    title = "Age-specific avoidable cancer mortality rates for females 30 to 75 years: 2007 to 2022",
    linetype = "Year"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8)) +
  scale_x_discrete(labels = c(
    "30" = "30-34",
    "35" = "35-39",
    "40" = "40-44",
    "45" = "45-49",
    "50" = "50-54",
    "55" = "55-59",
    "60" = "60-64",
    "65" = "65-69",
    "70" = "70-74"
  )) +
  scale_y_log10() 

avca_mx_all %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male",!age_group %in% c("0", "1", "5", "10", "15", "20", "25")) %>% 
  ggplot(aes(x = age_group, y = avca.mx, 
             linetype = factor(Year),
             group = interaction(Sex, Year))) +
  geom_line(colour = "skyblue3", size = 0.6) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Avoidable cancer mortality rate, log scale",
    title = "Age-specific avoidable cancer mortality rates for males 30 to 75 years: 2007 to 2022",
    linetype = "Year"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8)) +
  scale_x_discrete(labels = c(
    "30" = "30-34",
    "35" = "35-39",
    "40" = "40-44",
    "45" = "45-49",
    "50" = "50-54",
    "55" = "55-59",
    "60" = "60-64",
    "65" = "65-69",
    "70" = "70-74"
  )) +
  scale_y_log10() 




###################################################################
##making cause specific mx for each different avoidability group

data2007 <- data2007 %>% mutate(age_group = 
                                  recode(age_group, "<1" = "0", "1-4" = "1", "5-9" = "5", "10-14" = "10", "15-19" = "15", "20-24" = "20", "25-29" = "25",
                                         "30-34" = "30", "35-39" = "35", "40-44" = "40", "45-49" = "45", "50-54" = "50", "55-59" = "55", "60-64" = "60",
                                         "65-69" = "65", "70-74" = "70", "75-79" = "75", "80-84" = "80", ">85" = "85")) %>% 
  subset(select = -c(avoidable, total_deaths.o, total_deaths.c, proportion.o, proportion.c, proportion.b, death_type, deaths_pyramid)) 
##calculate cause spec mx
data2007 <-  data2007 %>% group_by(Country, Sex, age_group) %>% mutate(cause_mx = deaths/total_deaths.b) %>% ungroup() %>% mutate(Year = as.integer("2007"))

data2022 <- data2022 %>% mutate(age_group = 
                                  recode(age_group, "<1" = "0", "1-4" = "1", "5-9" = "5", "10-14" = "10", "15-19" = "15", "20-24" = "20", "25-29" = "25",
                                         "30-34" = "30", "35-39" = "35", "40-44" = "40", "45-49" = "45", "50-54" = "50", "55-59" = "55", "60-64" = "60",
                                         "65-69" = "65", "70-74" = "70", "75-79" = "75", "80-84" = "80", ">85" = "85")) %>% 
  subset(select = -c(avoidable, total_deaths.o, total_deaths.c, proportion.o, proportion.c, proportion.b, death_type, deaths_pyramid))
data2022 <-  data2022 %>% group_by(Country, Sex, age_group) %>% mutate(cause_mx = deaths/total_deaths.b) %>% ungroup() %>% mutate(Year = as.integer("2022"))
##make into joined for both years
data <- bind_rows(data2007, data2022) %>% mutate(age_group = as.integer(age_group))




FLT_ENG_WA <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"), !Age %in% c("90", "95", "100", "105", "110")) %>% mutate(Sex = "female")

MLT_ENG_WA <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"), !Age %in% c("90", "95", "100", "105", "110")) %>% mutate(Sex = "male")
ENG_WA <- bind_rows(MLT_ENG_WA, FLT_ENG_WA) %>% mutate(Country = "england_wales")
FLT_NIR <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"), !Age %in% c("90", "95", "100", "105", "110")) %>% mutate(Sex = "female")
MLT_NIR <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"), !Age %in% c("90", "95", "100", "105", "110")) %>% mutate(Sex = "male")
NIR <- bind_rows(MLT_NIR, FLT_NIR) %>% mutate(Country = "northern_irl")
FLT_IR <- readHMDweb(
  CNTRY = "IRL",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"), !Age %in% c("90", "95", "100", "105", "110")) %>% mutate(Sex = "female")
MLT_IR <- readHMDweb(
  CNTRY = "IRL",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"), !Age %in% c("90", "95", "100", "105", "110")) %>% mutate(Sex = "male")
IR <- bind_rows(MLT_IR, FLT_IR) %>% mutate(Country = "ireland")
FLT_SCO <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"), !Age %in% c("90", "95", "100", "105", "110")) %>% mutate(Sex = "female")
MLT_SCO <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007", "2022"), !Age %in% c("90", "95", "100", "105", "110")) %>% mutate(Sex = "male")
SCO <- bind_rows(MLT_SCO, FLT_SCO) %>% mutate(Country = "scotland")

allLT <- bind_rows(ENG_WA, SCO, NIR, IR)

##need to combine mx from lt data to causes spec counts
data_joined <- data %>% left_join(allLT, join_by(Year==Year, age_group==Age, Sex==Sex, Country==Country))
##multiply by age spec mx to get age-cause spec mx
data_joined <- data_joined %>% mutate(age_causemx = mx*cause_mx)

#now make a df with only age_causemx mx country year sex age group = start looking at decomp
data_age_causemx <- data_joined %>% subset(select = -c(total_deaths.b, deaths, cause_mx, mx, qx, ax, lx, dx, Lx, Tx, ex, OpenInterval))

export(data_age_causemx, file = "data/age_causemx.csv")
##maybe export the df with all the lt info too - might need it to calc later on
export(data_joined, file = "data/LT_age_causemx.csv")






############################################################################################
##need to make the 85 plus interval the open ended interval instead of just filtering it out

#######FIRST TO EW########
FLT_EW22 <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2022")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))
open_age <- 85
# rows below the new open interval: unchanged
fEW_22_below <- FLT_EW22 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_EW22_above <- FLT_EW22 %>% filter(Age == open_age) %>% pull(lx)
Tx_EW22_above <- FLT_EW22 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
fEW22above <- data.frame(
  Age = open_age,
  mx  = lx_EW22_above / Tx_EW22_above,
  qx  = 1,
  ax  = Tx_EW22_above / lx_EW22_above,
  lx  = lx_EW22_above,
  dx  = lx_EW22_above,
  Lx  = Tx_EW22_above,
  Tx  = Tx_EW22_above,
  ex  = Tx_EW22_above / lx_EW22_above,
  Year = 2022,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_EW22new <- bind_rows(fEW_22_below, fEW22above)
##seems to work - e85 is the same as in the original lt - 6.87
FLT_EW07 <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
fEW_07_below <- FLT_EW07 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_EW07_above <- FLT_EW07 %>% filter(Age == open_age) %>% pull(lx)
Tx_EW07_above <- FLT_EW07 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
fEW07above <- data.frame(
  Age = open_age,
  mx  = lx_EW07_above / Tx_EW07_above,
  qx  = 1,
  ax  = Tx_EW07_above / lx_EW07_above,
  lx  = lx_EW07_above,
  dx  = lx_EW07_above,
  Lx  = Tx_EW07_above,
  Tx  = Tx_EW07_above,
  ex  = Tx_EW07_above / lx_EW07_above,
  Year = 2007,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_EW07new <- bind_rows(fEW_07_below, fEW07above)
FLT_EWnew <- bind_rows(FLT_EW07new, FLT_EW22new)|> mutate(Country = "england_wales")

##now doing the same for EWmales
MLT_EW22 <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2022")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MEW_22_below <- MLT_EW22 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_EW22_above <- MLT_EW22 %>% filter(Age == open_age) %>% pull(lx)
MTx_EW22_above <- MLT_EW22 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MEW22above <- data.frame(
  Age = open_age,
  mx  = Mlx_EW22_above / MTx_EW22_above,
  qx  = 1,
  ax  = MTx_EW22_above / Mlx_EW22_above,
  lx  = Mlx_EW22_above,
  dx  = Mlx_EW22_above,
  Lx  = MTx_EW22_above,
  Tx  = MTx_EW22_above,
  ex  = MTx_EW22_above / Mlx_EW22_above,
  Year = 2022,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_EW22new <- bind_rows(MEW_22_below, MEW22above)
##seems to work - e85 is the same as in the original lt - 6.87
MLT_EW07 <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MEW_07_below <- MLT_EW07 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_EW07_above <- MLT_EW07 %>% filter(Age == open_age) %>% pull(lx)
MTx_EW07_above <- MLT_EW07 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MEW07above <- data.frame(
  Age = open_age,
  mx  = Mlx_EW07_above / MTx_EW07_above,
  qx  = 1,
  ax  = MTx_EW07_above / Mlx_EW07_above,
  lx  = Mlx_EW07_above,
  dx  = Mlx_EW07_above,
  Lx  = MTx_EW07_above,
  Tx  = MTx_EW07_above,
  ex  = MTx_EW07_above / Mlx_EW07_above,
  Year = 2007,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_EW07new <- bind_rows(MEW_07_below, MEW07above)
MLT_EWnew <- bind_rows(MLT_EW07new, MLT_EW22new)|> mutate(Country = "england_wales")

#####NEXT FOR NIR#######

FLT_NI22 <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2022")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))
open_age <- 85
# rows below the new open interval: unchanged
fNI_22_below <- FLT_NI22 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FNI22_above <- FLT_NI22 %>% filter(Age == open_age) %>% pull(lx)
Tx_FNI22_above <- FLT_NI22 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FNI22above <- data.frame(
  Age = open_age,
  mx  = lx_FNI22_above / Tx_FNI22_above,
  qx  = 1,
  ax  = Tx_FNI22_above / lx_FNI22_above,
  lx  = lx_FNI22_above,
  dx  = lx_FNI22_above,
  Lx  = Tx_FNI22_above,
  Tx  = Tx_FNI22_above,
  ex  = Tx_FNI22_above / lx_FNI22_above,
  Year = 2022,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_NI22new <- bind_rows(fNI_22_below, FNI22above)
##seems to work - e85 is the same as in the original lt - 6.87
FLT_NI07 <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
FNI_07_below <- FLT_NI07 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FNI07_above <- FLT_NI07 %>% filter(Age == open_age) %>% pull(lx)
Tx_FNI07_above <- FLT_NI07 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FNI07above <- data.frame(
  Age = open_age,
  mx  = lx_FNI07_above / Tx_FNI07_above,
  qx  = 1,
  ax  = Tx_FNI07_above / lx_FNI07_above,
  lx  = lx_FNI07_above,
  dx  = lx_FNI07_above,
  Lx  = Tx_FNI07_above,
  Tx  = Tx_FNI07_above,
  ex  = Tx_FNI07_above / lx_FNI07_above,
  Year = 2007,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_FNI07new <- bind_rows(FNI_07_below, FNI07above)
FLT_NInew <- bind_rows(FLT_FNI07new, FLT_NI22new)|> mutate(Country = "northern_irl")

##now doing the same for NIR Males
MLT_NI22 <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2022")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MNI_22_below <- MLT_NI22 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_NI22_above <- MLT_NI22 %>% filter(Age == open_age) %>% pull(lx)
MTx_NI22_above <- MLT_NI22 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MNI22above <- data.frame(
  Age = open_age,
  mx  = Mlx_NI22_above / MTx_NI22_above,
  qx  = 1,
  ax  = MTx_NI22_above / Mlx_NI22_above,
  lx  = Mlx_NI22_above,
  dx  = Mlx_NI22_above,
  Lx  = MTx_NI22_above,
  Tx  = MTx_NI22_above,
  ex  = MTx_NI22_above / Mlx_NI22_above,
  Year = 2022,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_NI22new <- bind_rows(MNI_22_below, MNI22above)

MLT_NI07 <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MNI_07_below <- MLT_NI07 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_NI07_above <- MLT_NI07 %>% filter(Age == open_age) %>% pull(lx)
MTx_NI07_above <- MLT_NI07 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MNI07above <- data.frame(
  Age = open_age,
  mx  = Mlx_NI07_above / MTx_NI07_above,
  qx  = 1,
  ax  = MTx_NI07_above / Mlx_NI07_above,
  lx  = Mlx_NI07_above,
  dx  = Mlx_NI07_above,
  Lx  = MTx_NI07_above,
  Tx  = MTx_NI07_above,
  ex  = MTx_NI07_above / Mlx_NI07_above,
  Year = 2007,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_NI07new <- bind_rows(MNI_07_below, MNI07above)
MLT_NInew <- bind_rows(MLT_NI07new, MLT_NI22new)|> mutate(Country = "northern_irl")

#############now for sco
FLT_SC22 <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2022")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))
open_age <- 85
# rows below the new open interval: unchanged
fSC_22_below <- FLT_SC22 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FSC22_above <- FLT_SC22 %>% filter(Age == open_age) %>% pull(lx)
Tx_FSC22_above <- FLT_SC22 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FSC22above <- data.frame(
  Age = open_age,
  mx  = lx_FSC22_above / Tx_FSC22_above,
  qx  = 1,
  ax  = Tx_FSC22_above / lx_FSC22_above,
  lx  = lx_FSC22_above,
  dx  = lx_FSC22_above,
  Lx  = Tx_FSC22_above,
  Tx  = Tx_FSC22_above,
  ex  = Tx_FSC22_above / lx_FSC22_above,
  Year = 2022,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_SC22new <- bind_rows(fSC_22_below, FSC22above)
##seems to work - e85 is the same as in the original lt - 6.87
FLT_SC07 <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
FSC_07_below <- FLT_SC07 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FSC07_above <- FLT_SC07 %>% filter(Age == open_age) %>% pull(lx)
Tx_FSC07_above <- FLT_SC07 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FSC07above <- data.frame(
  Age = open_age,
  mx  = lx_FSC07_above / Tx_FSC07_above,
  qx  = 1,
  ax  = Tx_FSC07_above / lx_FSC07_above,
  lx  = lx_FSC07_above,
  dx  = lx_FSC07_above,
  Lx  = Tx_FSC07_above,
  Tx  = Tx_FSC07_above,
  ex  = Tx_FSC07_above / lx_FSC07_above,
  Year = 2007,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_FSC07new <- bind_rows(FSC_07_below, FSC07above)
FLT_SCnew <- bind_rows(FLT_FSC07new, FLT_SC22new)|> mutate(Country = "scotland")

##now doing the same for SCO Males
MLT_SC22 <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2022")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MSC_22_below <- MLT_SC22 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_SC22_above <- MLT_SC22 %>% filter(Age == open_age) %>% pull(lx)
MTx_SC22_above <- MLT_SC22 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MSC22above <- data.frame(
  Age = open_age,
  mx  = Mlx_SC22_above / MTx_SC22_above,
  qx  = 1,
  ax  = MTx_SC22_above / Mlx_SC22_above,
  lx  = Mlx_SC22_above,
  dx  = Mlx_SC22_above,
  Lx  = MTx_SC22_above,
  Tx  = MTx_SC22_above,
  ex  = MTx_SC22_above / Mlx_SC22_above,
  Year = 2022,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_SC22new <- bind_rows(MSC_22_below, MSC22above)

MLT_SC07 <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MSC_07_below <- MLT_SC07 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_SC07_above <- MLT_SC07 %>% filter(Age == open_age) %>% pull(lx)
MTx_SC07_above <- MLT_SC07 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MSC07above <- data.frame(
  Age = open_age,
  mx  = Mlx_SC07_above / MTx_SC07_above,
  qx  = 1,
  ax  = MTx_SC07_above / Mlx_SC07_above,
  lx  = Mlx_SC07_above,
  dx  = Mlx_SC07_above,
  Lx  = MTx_SC07_above,
  Tx  = MTx_SC07_above,
  ex  = MTx_SC07_above / Mlx_SC07_above,
  Year = 2007,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_SC07new <- bind_rows(MSC_07_below, MSC07above)
MLT_SCnew <- bind_rows(MLT_SC07new, MLT_SC22new) |> mutate(Country = "scotland")

############FINALLY FOR IRL
FLT_IR22 <- readHMDweb(
  CNTRY = "IRL",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2022")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))
open_age <- 85
# rows below the new open interval: unchanged
fIR_22_below <- FLT_IR22 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FIR22_above <- FLT_IR22 %>% filter(Age == open_age) %>% pull(lx)
Tx_FIR22_above <- FLT_IR22 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FIR22above <- data.frame(
  Age = open_age,
  mx  = lx_FIR22_above / Tx_FIR22_above,
  qx  = 1,
  ax  = Tx_FIR22_above / lx_FIR22_above,
  lx  = lx_FIR22_above,
  dx  = lx_FIR22_above,
  Lx  = Tx_FIR22_above,
  Tx  = Tx_FIR22_above,
  ex  = Tx_FIR22_above / lx_FIR22_above,
  Year = 2022,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_IR22new <- bind_rows(fIR_22_below, FIR22above)
##seems to work - e85 is the same as in the original lt - 6.87
FLT_IR07 <- readHMDweb(
  CNTRY = "IRL",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
FIR_07_below <- FLT_IR07 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FIR07_above <- FLT_IR07 %>% filter(Age == open_age) %>% pull(lx)
Tx_FIR07_above <- FLT_IR07 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FIR07above <- data.frame(
  Age = open_age,
  mx  = lx_FIR07_above / Tx_FIR07_above,
  qx  = 1,
  ax  = Tx_FIR07_above / lx_FIR07_above,
  lx  = lx_FIR07_above,
  dx  = lx_FIR07_above,
  Lx  = Tx_FIR07_above,
  Tx  = Tx_FIR07_above,
  ex  = Tx_FIR07_above / lx_FIR07_above,
  Year = 2007,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_FIR07new <- bind_rows(FIR_07_below, FIR07above)
FLT_IRnew <- bind_rows(FLT_FIR07new, FLT_IR22new) |> mutate(Country = "ireland")

##now doing the same for IRL Males
MLT_IR22 <- readHMDweb(
  CNTRY = "IRL",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2022")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MIR_22_below <- MLT_IR22 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_IR22_above <- MLT_IR22 %>% filter(Age == open_age) %>% pull(lx)
MTx_IR22_above <- MLT_IR22 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MIR22above <- data.frame(
  Age = open_age,
  mx  = Mlx_IR22_above / MTx_IR22_above,
  qx  = 1,
  ax  = MTx_IR22_above / Mlx_IR22_above,
  lx  = Mlx_IR22_above,
  dx  = Mlx_IR22_above,
  Lx  = MTx_IR22_above,
  Tx  = MTx_IR22_above,
  ex  = MTx_IR22_above / Mlx_IR22_above,
  Year = 2022,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_IR22new <- bind_rows(MIR_22_below, MIR22above)

MLT_IR07 <- readHMDweb(
  CNTRY = "IRL",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2007")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MIR_07_below <- MLT_IR07 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_IR07_above <- MLT_IR07 %>% filter(Age == open_age) %>% pull(lx)
MTx_IR07_above <- MLT_IR07 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MIR07above <- data.frame(
  Age = open_age,
  mx  = Mlx_IR07_above / MTx_IR07_above,
  qx  = 1,
  ax  = MTx_IR07_above / Mlx_IR07_above,
  lx  = Mlx_IR07_above,
  dx  = Mlx_IR07_above,
  Lx  = MTx_IR07_above,
  Tx  = MTx_IR07_above,
  ex  = MTx_IR07_above / Mlx_IR07_above,
  Year = 2007,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_IR07new <- bind_rows(MIR_07_below, MIR07above)
MLT_IRnew <- bind_rows(MLT_IR07new, MLT_IR22new) |> mutate(Country = "ireland")

allLTNEW <- bind_rows(FLT_EWnew, FLT_SCnew, FLT_NInew, FLT_IRnew,
                      MLT_EWnew, MLT_SCnew, MLT_NInew, MLT_IRnew)

##need to combine mx from lt data to causes spec counts
data_joined_NEW <- data %>% left_join(allLTNEW, join_by(Year==Year, age_group==Age, Sex==Sex, Country==Country))
##multiply by age spec mx to get age-cause spec mx
data_joined_NEW <- data_joined_NEW %>% mutate(age_causemx = mx*cause_mx)

#now make a df with only age_causemx mx country year sex age group = start looking at decomp
data_age_causemxNEW <- data_joined_NEW %>% subset(select = -c(total_deaths.b, deaths, cause_mx, mx, qx, ax, lx, dx, Lx, Tx, ex, OpenInterval))

export(data_age_causemxNEW, file = "data/NEWage_causemx.csv")
##maybe export the df with all the lt info too - might need it to calc later on
export(data_joined, file = "data/LT_age_causemx.csv")



#####################################################
##doing same for 2019
data2019 <- import(file = "data/avoidable_deaths_19.csv")
############################################################################################
##need to make the 85 plus interval the open ended interval instead of just filtering it out

#######FIRST TO EW########
FLT_EW19 <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2019")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))
open_age <- 85
# rows below the new open interval: unchanged
fEW_19_below <- FLT_EW19 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_EW19_above <- FLT_EW19 %>% filter(Age == open_age) %>% pull(lx)
Tx_EW19_above <- FLT_EW19 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
fEW19above <- data.frame(
  Age = open_age,
  mx  = lx_EW19_above / Tx_EW19_above,
  qx  = 1,
  ax  = Tx_EW19_above / lx_EW19_above,
  lx  = lx_EW19_above,
  dx  = lx_EW19_above,
  Lx  = Tx_EW19_above,
  Tx  = Tx_EW19_above,
  ex  = Tx_EW19_above / lx_EW19_above,
  Year = 2019,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_EW19new <- bind_rows(fEW_19_below, fEW19above)|> mutate(country = "england_wales")

##now doing the same for EWmales
MLT_EW19 <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2019")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MEW_19_below <- MLT_EW19 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_EW19_above <- MLT_EW19 %>% filter(Age == open_age) %>% pull(lx)
MTx_EW19_above <- MLT_EW19 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MEW19above <- data.frame(
  Age = open_age,
  mx  = Mlx_EW19_above / MTx_EW19_above,
  qx  = 1,
  ax  = MTx_EW19_above / Mlx_EW19_above,
  lx  = Mlx_EW19_above,
  dx  = Mlx_EW19_above,
  Lx  = MTx_EW19_above,
  Tx  = MTx_EW19_above,
  ex  = MTx_EW19_above / Mlx_EW19_above,
  Year = 2019,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_EW19new <- bind_rows(MEW_19_below, MEW19above)|> mutate(country = "england_wales")

#####NEXT FOR NIR#######

FLT_NI19 <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2019")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))
open_age <- 85
# rows below the new open interval: unchanged
fNI_19_below <- FLT_NI19 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FNI19_above <- FLT_NI19 %>% filter(Age == open_age) %>% pull(lx)
Tx_FNI19_above <- FLT_NI19 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FNI19above <- data.frame(
  Age = open_age,
  mx  = lx_FNI19_above / Tx_FNI19_above,
  qx  = 1,
  ax  = Tx_FNI19_above / lx_FNI19_above,
  lx  = lx_FNI19_above,
  dx  = lx_FNI19_above,
  Lx  = Tx_FNI19_above,
  Tx  = Tx_FNI19_above,
  ex  = Tx_FNI19_above / lx_FNI19_above,
  Year = 2019,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_NI19new <- bind_rows(fNI_19_below, FNI19above)|> mutate(country = "northern_irl")

##now doing the same for NIR Males
MLT_NI19 <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2019")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MNI_19_below <- MLT_NI19 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_NI19_above <- MLT_NI19 %>% filter(Age == open_age) %>% pull(lx)
MTx_NI19_above <- MLT_NI19 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MNI19above <- data.frame(
  Age = open_age,
  mx  = Mlx_NI19_above / MTx_NI19_above,
  qx  = 1,
  ax  = MTx_NI19_above / Mlx_NI19_above,
  lx  = Mlx_NI19_above,
  dx  = Mlx_NI19_above,
  Lx  = MTx_NI19_above,
  Tx  = MTx_NI19_above,
  ex  = MTx_NI19_above / Mlx_NI19_above,
  Year = 2019,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_NI19new <- bind_rows(MNI_19_below, MNI19above)|> mutate(country = "northern_irl")


#############now for sco
FLT_SC19 <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2019")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))
open_age <- 85
# rows below the new open interval: unchanged
fSC_19_below <- FLT_SC19 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FSC19_above <- FLT_SC19 %>% filter(Age == open_age) %>% pull(lx)
Tx_FSC19_above <- FLT_SC19 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FSC19above <- data.frame(
  Age = open_age,
  mx  = lx_FSC19_above / Tx_FSC19_above,
  qx  = 1,
  ax  = Tx_FSC19_above / lx_FSC19_above,
  lx  = lx_FSC19_above,
  dx  = lx_FSC19_above,
  Lx  = Tx_FSC19_above,
  Tx  = Tx_FSC19_above,
  ex  = Tx_FSC19_above / lx_FSC19_above,
  Year = 2019,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_SC19new <- bind_rows(fSC_19_below, FSC19above)|> mutate(country = "scotland")

##now doing the same for SCO Males
MLT_SC19 <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2019")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MSC_19_below <- MLT_SC19 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_SC19_above <- MLT_SC19 %>% filter(Age == open_age) %>% pull(lx)
MTx_SC19_above <- MLT_SC19 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MSC19above <- data.frame(
  Age = open_age,
  mx  = Mlx_SC19_above / MTx_SC19_above,
  qx  = 1,
  ax  = MTx_SC19_above / Mlx_SC19_above,
  lx  = Mlx_SC19_above,
  dx  = Mlx_SC19_above,
  Lx  = MTx_SC19_above,
  Tx  = MTx_SC19_above,
  ex  = MTx_SC19_above / Mlx_SC19_above,
  Year = 2019,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_SC19new <- bind_rows(MSC_19_below, MSC19above) |> mutate(country = "scotland")

############FINALLY FOR IRL
FLT_IR19 <- readHMDweb(
  CNTRY = "IRL",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2019")) %>% mutate(Sex = "female") |> mutate(Age = as.numeric(Age))
open_age <- 85
# rows below the new open interval: unchanged
fIR_19_below <- FLT_IR19 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
lx_FIR19_above <- FLT_IR19 %>% filter(Age == open_age) %>% pull(lx)
Tx_FIR19_above <- FLT_IR19 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
FIR19above <- data.frame(
  Age = open_age,
  mx  = lx_FIR19_above / Tx_FIR19_above,
  qx  = 1,
  ax  = Tx_FIR19_above / lx_FIR19_above,
  lx  = lx_FIR19_above,
  dx  = lx_FIR19_above,
  Lx  = Tx_FIR19_above,
  Tx  = Tx_FIR19_above,
  ex  = Tx_FIR19_above / lx_FIR19_above,
  Year = 2019,
  OpenInterval = TRUE,
  Sex = "female"
)
FLT_IR19new <- bind_rows(fIR_19_below, FIR19above) |> mutate(country = "ireland")

##now doing the same for IRL Males
MLT_IR19 <- readHMDweb(
  CNTRY = "IRL",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
) %>% filter(Year %in% c("2019")) %>% mutate(Sex = "male") |> mutate(Age = as.numeric(Age))

# rows below the new open interval: unchanged
MIR_19_below <- MLT_IR19 %>% filter(Age < open_age)

# pull lx and Tx exactly at the new open age
Mlx_IR19_above <- MLT_IR19 %>% filter(Age == open_age) %>% pull(lx)
MTx_IR19_above <- MLT_IR19 %>% filter(Age == open_age) %>% pull(Tx)

# build the new 85+ row
MIR19above <- data.frame(
  Age = open_age,
  mx  = Mlx_IR19_above / MTx_IR19_above,
  qx  = 1,
  ax  = MTx_IR19_above / Mlx_IR19_above,
  lx  = Mlx_IR19_above,
  dx  = Mlx_IR19_above,
  Lx  = MTx_IR19_above,
  Tx  = MTx_IR19_above,
  ex  = MTx_IR19_above / Mlx_IR19_above,
  Year = 2019,
  OpenInterval = TRUE,
  Sex = "male"
)
MLT_IR19new <- bind_rows(MIR_19_below, MIR19above) |> mutate(country = "ireland")

allLT19 <- bind_rows(FLT_EW19new, FLT_SC19new, FLT_NI19new, FLT_IR19new,
                      MLT_EW19new, MLT_SC19new, MLT_NI19new, MLT_IR19new)
allLT19 <- allLT19 |> mutate(Year = as.character(Year))
##need to combine mx from lt data to causes spec counts
##first calc cause_mx
data2019 <- data2019 %>%  
  subset(select = -c(avoidable, total_deaths.o, total_deaths.c, proportion.o, proportion.c, proportion.b, death_type, deaths_pyramid)) 
##calculate cause spec mx
data2019 <-  data2019 %>% group_by(Country, Sex, age_group) %>% mutate(cause_mx = deaths/total_deaths.b) %>% ungroup() %>% mutate(Year = as.integer("2019"))

data2019 <- data2019 |> mutate(Year= "2019")
data_joined_19 <- data2019 %>% left_join(allLT19, join_by(Year==Year, age_group==Age, Sex==Sex, Country==country))
##multiply by age spec mx to get age-cause spec mx
data_joined_19 <- data_joined_19 %>% mutate(age_causemx = mx*cause_mx)

#now make a df with only age_causemx mx country year sex age group = start looking at decomp
data_age_causemx19 <- data_joined_19 %>% subset(select = -c(total_deaths.b, deaths, cause_mx, mx, qx, ax, lx, dx, Lx, Tx, ex, OpenInterval))

export(data_age_causemx19, file = "data/2019age_causemx.csv")
#
