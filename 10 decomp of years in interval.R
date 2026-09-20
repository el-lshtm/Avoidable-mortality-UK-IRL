####part 10 average years lived between 0 and 75 - calculation and decomposition


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
library(ggtext)
myHMDusername <- "insert-user"
myHMDpassword <- "insert-password"

cause_names<-c("1"="50/50 cancer", "2"="50/50 other","3"="not avoidable cancer",
               "4"="not avoidable other","5"="preventable cancer",
               "6"= "preventable other","7"="treatable cancer",
               "8"="treatable other")
age_names<-c("0"="0","1"="1-4","5"="5-9","10"="10-14","15"="15-19","20"="20-24",
             "25"="25-29","30"="30-34","35"="35-39","40"="40-44","45"="45-49",
             "50"="50-54","55"="55-59","60"="60-64","65"="65-69","70"="70-74",
             "75"="75-79","80"="80-84","85"="85+")    

##import age cause specific mx and reformat for decomp
NEWage_causemx <- import(file = "data/NEWage_causemx.csv")

NEWage_causemx_wide <- NEWage_causemx %>% 
  pivot_wider(
    names_from = fill_group,
    values_from = age_causemx
  ) %>% clean_names() %>% replace(is.na(.), 0)

NEWage_causemx_wide <- NEWage_causemx_wide %>%
  relocate(country, sex, age_group, year, .after = last_col()) %>%
  rename(
    `1` = x50_50_cancer,
    `2` = x50_50_other,
    `3` = not_avoidable_cancer,
    `4` = not_avoidable_other,
    `5` = preventable_cancer,
    `6` = preventable_other,
    `7` = treatable_cancer,
    `8` = treatable_other
  ) 

##need it to be in order of age group so vectors of mx are in correct order
NEWage_causemx_wide <- NEWage_causemx_wide %>% arrange(age_group, country, sex, year)


####importing lt data to calculate average years lived between 0 and 75
FLT_ENG_WA <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)
MLT_ENG_WA <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)

FLT_ENG_WA <- FLT_ENG_WA %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "female")
MLT_ENG_WA <- MLT_ENG_WA %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "male")

ENG_WA <- bind_rows(FLT_ENG_WA, MLT_ENG_WA) %>% mutate(Country = "england_wales")


##compare with ireland
FLT_IRL <- readHMDweb(
  CNTRY = "IRL",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)
MLT_IRL <- readHMDweb(
  CNTRY = "IRL",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)

FLT_IRL <- FLT_IRL %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "female")
MLT_IRL <- MLT_IRL %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "male")
IRL <- bind_rows(FLT_IRL, MLT_IRL) %>% mutate(Country = "ireland")


FLT_SCO <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)
MLT_SCO <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)

FLT_SCO <- FLT_SCO %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "female")
MLT_SCO <- MLT_SCO %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "male")
SCO <- bind_rows(FLT_SCO, MLT_SCO) %>% mutate(Country = "scotland")
FLT_NIR <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "fltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)
MLT_NIR <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "mltper_5x1",
  username = myHMDusername,
  password = myHMDpassword
)

FLT_NIR <- FLT_NIR %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "female")
MLT_NIR <- MLT_NIR %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_0_75 = (Tx[Age == 0] - Tx[Age == 75])/lx[Age ==0]
  ) %>% mutate(Sex = "male")
NIR <- bind_rows(FLT_NIR, MLT_NIR) %>% mutate(Country = "northern_irl")

all <- bind_rows(ENG_WA, SCO, IRL, NIR)


label_data <- all %>%
  filter(Year %in% c(2007, 2022))
country_labs <- c("england_wales" = "England & Wales", "ireland" = "Republic of Ireland",
                  "northern_irl" = "Northern Ireland", "scotland" = "Scotland")

scale_x_continuous(breaks = seq(2007, 2023, by = 2))

all %>% ggplot(aes(x = Year, y = PY_0_75, color = Sex)) +
  geom_line(linewidth = 0.8) + coord_cartesian(ylim = c(67.5, 75), xlim = c(2005, 2024)) + scale_x_continuous(breaks = seq(2007, 2022, by = 2)) +
  geom_point() +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  scale_color_manual(name = "Sex", values = c("male" = "skyblue3", "female" = "violetred3")) +
  labs(
    x = "Year", 
    y = "Average years lived between 0 and 75",
    title = "Average number of years lived between 0 and 75,
    in UK & Ireland, 2007 to 2022") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + geom_text(
    data = label_data,
    aes(label = round(PY_0_75, 2)),
    vjust = -0.8,
    size = 4.5
  )

##make a table for the two years of interest

library(gt)
label_data |> gt()

label_data %>% 
  gt(groupname_col = "Country") %>% 
  tab_header(
    title = "Average years lived between 0 and 75",
    subtitle = "2007 and 2022"
  ) |>
  cols_label(
    PY_0_75 = "Person-Years (0-75)",
    Sex = "Sex",
    Year = "Year"
  ) |>
  fmt_number(columns = PY_0_75, decimals = 3)


##############################################################################
##making function for PY_0_75 to use in horiuchi decomp
##function calculating e0 using 2 vectors of mx
PY_0_75 <- function(nmx =  mx, sex=1, age = c(0, 1, seq(5, 75, 5)), nax = NULL){
  n   <- c(diff(age), 999)
  
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == 1) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == 2) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  AVERAGE <- Tx[1]-Tx[17]/lx[1] #to calc average years lived in the interval- T0-T75/l0
  e0 <- AVERAGE[1]
  
  return(e0)
}

PY075 <- function(mxcvec,sex=1){
  dim(mxcvec) <- c(19,length(mxcvec)/19) #correct number of rows - 17 rows,  0 to 75?
  mx          <- rowSums(mxcvec)
  PY_0_75(mx,sex)
}
###AND FOR FEMALES?
PY_0_75F <- function(nmx =  mx, sex=2, age = c(0, 1, seq(5, 75, 5)), nax = NULL){
  n   <- c(diff(age), 999)
  
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == 1) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == 2) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  AVERAGE <- Tx[1]-Tx[17]/lx[1] #to calc average years lived in the interval- T0-T75/l0
  e0 <- AVERAGE[1]
  
  return(e0)
}

PY075F <- function(mxcvec,sex=2){
  dim(mxcvec) <- c(19,length(mxcvec)/19) #correct number of rows - 17 rows,  0 to 75?
  mx          <- rowSums(mxcvec)
  PY_0_75F(mx,sex)
}

##try filtering to ew males into 2 vectors - put into horiuchi funciton
EW_men <- NEWage_causemx_wide %>% filter(country=="england_wales", sex=="male")
EWM_COD07        <- as.matrix(EW_men[EW_men$year==2007,1:8])
EWM_COD22       <- as.matrix(EW_men[EW_men$year==2022,1:8])
EWM_Results <- horiuchi(func = PY075, pars1 = c(EWM_COD07), pars2 = c(EWM_COD22), N = 100)
dim(EWM_Results) <- dim(EWM_COD07)

##makes the decomp results into a df that can be plotted
EWM_Results            <- data.frame(EWM_Results)
colnames(EWM_Results)  <- cause_names
EWM_Results$Age        <- c(0,1,seq(5,85,5))
rownames(EWM_Results)  <- age_names
EWM_Results            <- gather(data = EWM_Results,key = Cause,value = Contribution,-Age) 
EWM_Results <- dplyr::filter(EWM_Results, !Age %in% c(75, 80, 85))

#now graph results
EW_M <-ggplot(data=EWM_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("England & Wales") +
  scale_fill_paletteer_d("tvthemes::Alexandrite") + labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()

##calc total contribution by cause
EWM_Results_sum <- EWM_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "england_wales", Sex = "male")

##for ew females
EW_women <- NEWage_causemx_wide %>% filter(country=="england_wales", sex=="female")
EWF_COD07        <- as.matrix(EW_women[EW_women$year==2007,1:8])
EWF_COD22       <- as.matrix(EW_women[EW_women$year==2022,1:8])
EWF_Results <- horiuchi(func = PY075F, pars1 = c(EWF_COD07), pars2 = c(EWF_COD22), N = 100)
dim(EWF_Results) <- dim(EWF_COD07)

##makes the decomp results into a df that can be plotted
EWF_Results            <- data.frame(EWF_Results)
colnames(EWF_Results)  <- cause_names
EWF_Results$Age        <- c(0,1,seq(5,85,5))
rownames(EWF_Results)  <- age_names
EWF_Results            <- gather(data = EWF_Results,key = Cause,value = Contribution,-Age) 
EWF_Results <- dplyr::filter(EWF_Results, !Age %in% c(75, 80, 85))

#now graph results
EW_F <- ggplot(data=EWF_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("England & Wales") +
  scale_fill_paletteer_d("tvthemes::Alexandrite") + labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown())+theme_minimal()

##calc total contribution by cause
EWF_Results_sum <- EWF_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "england_wales", Sex = "female")

#######IRL
IR_men <- NEWage_causemx_wide %>% filter(country=="ireland", sex=="male")
IRM_COD07        <- as.matrix(IR_men[IR_men$year==2007,1:8])
IRM_COD22       <- as.matrix(IR_men[IR_men$year==2022,1:8])
IRM_Results <- horiuchi(func = PY075, pars1 = c(IRM_COD07), pars2 = c(IRM_COD22), N = 100)
dim(IRM_Results) <- dim(IRM_COD07)

##makes the decomp results into a df that can be plotted
IRM_Results            <- data.frame(IRM_Results)
colnames(IRM_Results)  <- cause_names
IRM_Results$Age        <- c(0,1,seq(5,85,5))
rownames(IRM_Results)  <- age_names
IRM_Results            <- gather(data = IRM_Results,key = Cause,value = Contribution,-Age) 
IRM_Results <- dplyr::filter(IRM_Results, !Age %in% c(75, 80, 85))

#now graph results
IR_M <- ggplot(data=IRM_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Ireland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") + labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()

##calc total contribution by cause
IRM_Results_sum <- IRM_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "ireland", Sex = "male")

##for IR females
IR_women <- NEWage_causemx_wide %>% filter(country=="ireland", sex=="female")
IRF_COD07        <- as.matrix(IR_women[IR_women$year==2007,1:8])
IRF_COD22       <- as.matrix(IR_women[IR_women$year==2022,1:8])
IRF_Results <- horiuchi(func = PY075F, pars1 = c(IRF_COD07), pars2 = c(IRF_COD22), N = 100)
dim(IRF_Results) <- dim(IRF_COD07)

##makes the decomp results into a df that can be plotted
IRF_Results            <- data.frame(IRF_Results)
colnames(IRF_Results)  <- cause_names
IRF_Results$Age        <- c(0,1,seq(5,85,5))
rownames(IRF_Results)  <- age_names
IRF_Results            <- gather(data = IRF_Results,key = Cause,value = Contribution,-Age) 
IRF_Results <- dplyr::filter(IRF_Results, !Age %in% c(75, 80, 85))

#now graph results
IR_F <- ggplot(data=IRF_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Ireland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") + labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()

##calc total contribution by cause
IRF_Results_sum <- IRF_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "ireland", Sex = "female")

##########for sco
SC_men <- NEWage_causemx_wide %>% filter(country=="scotland", sex=="male")
SCM_COD07        <- as.matrix(SC_men[SC_men$year==2007,1:8])
SCM_COD22       <- as.matrix(SC_men[SC_men$year==2022,1:8])
SCM_Results <- horiuchi(func = PY075, pars1 = c(SCM_COD07), pars2 = c(SCM_COD22), N = 100)
dim(SCM_Results) <- dim(SCM_COD07)

##makes the decomp results into a df that can be plotted
SCM_Results            <- data.frame(SCM_Results)
colnames(SCM_Results)  <- cause_names
SCM_Results$Age        <- c(0,1,seq(5,85,5))
rownames(SCM_Results)  <- age_names
SCM_Results            <- gather(data = SCM_Results,key = Cause,value = Contribution,-Age) 
SCM_Results <- dplyr::filter(SCM_Results, !Age %in% c(75, 80, 85))

#now graph results
SC_M <- ggplot(data=SCM_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Scotland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") + labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()

##calc total contribution by cause
SCM_Results_sum <- SCM_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "scotland", Sex = "male")

##for IR females
SC_women <- NEWage_causemx_wide %>% filter(country=="scotland", sex=="female")
SCF_COD07        <- as.matrix(SC_women[SC_women$year==2007,1:8])
SCF_COD22       <- as.matrix(SC_women[SC_women$year==2022,1:8])
SCF_Results <- horiuchi(func = PY075F, pars1 = c(SCF_COD07), pars2 = c(SCF_COD22), N = 100)
dim(SCF_Results) <- dim(SCF_COD07)

##makes the decomp results into a df that can be plotted
SCF_Results            <- data.frame(SCF_Results)
colnames(SCF_Results)  <- cause_names
SCF_Results$Age        <- c(0,1,seq(5,85,5))
rownames(SCF_Results)  <- age_names
SCF_Results            <- gather(data = SCF_Results,key = Cause,value = Contribution,-Age) 
SCF_Results <- dplyr::filter(SCF_Results, !Age %in% c(75, 80, 85))

#now graph results
SC_F <- ggplot(data=SCF_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Scotland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") + labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()

##calc total contribution by cause
SCF_Results_sum <- SCF_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "scotland", Sex = "female")


#######and nir
NI_men <- NEWage_causemx_wide %>% filter(country=="northern_irl", sex=="male")
NIM_COD07        <- as.matrix(NI_men[NI_men$year==2007,1:8])
NIM_COD22       <- as.matrix(NI_men[NI_men$year==2022,1:8])
NIM_Results <- horiuchi(func = PY075, pars1 = c(NIM_COD07), pars2 = c(NIM_COD22), N = 100)
dim(NIM_Results) <- dim(NIM_COD07)

##makes the decomp results into a df that can be plotted
NIM_Results            <- data.frame(NIM_Results)
colnames(NIM_Results)  <- cause_names
NIM_Results$Age        <- c(0,1,seq(5,85,5))
rownames(NIM_Results)  <- age_names
NIM_Results            <- gather(data = NIM_Results,key = Cause,value = Contribution,-Age) 
NIM_Results <- dplyr::filter(NIM_Results, !Age %in% c(75, 80, 85))

#now graph results
NI_M <- ggplot(data=NIM_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Northern Ireland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") + labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()

##calc total contribution by cause
NIM_Results_sum <- NIM_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "northern_irl", Sex = "male")

##for IR females
NI_women <- NEWage_causemx_wide %>% filter(country=="northern_irl", sex=="female")
NIF_COD07        <- as.matrix(NI_women[NI_women$year==2007,1:8])
NIF_COD22       <- as.matrix(NI_women[NI_women$year==2022,1:8])
NIF_Results <- horiuchi(func = PY075F, pars1 = c(NIF_COD07), pars2 = c(NIF_COD22), N = 100)
dim(NIF_Results) <- dim(NIF_COD07)

##makes the decomp results into a df that can be plotted
NIF_Results            <- data.frame(NIF_Results)
colnames(NIF_Results)  <- cause_names
NIF_Results$Age        <- c(0,1,seq(5,85,5))
rownames(NIF_Results)  <- age_names
NIF_Results            <- gather(data = NIF_Results,key = Cause,value = Contribution,-Age) 
NIF_Results <- dplyr::filter(NIF_Results, !Age %in% c(75, 80, 85))

#now graph results
NI_F <- ggplot(data=NIF_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Northern Ireland")+ 
  scale_fill_paletteer_d("tvthemes::Alexandrite") +  labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()

##calc total contribution by cause
NIF_Results_sum <- NIF_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "northern_irl", Sex = "female")


###########LOOK AT SUMMED RESULTS
sum_results <- bind_rows(NIF_Results_sum, NIM_Results_sum, EWM_Results_sum, EWF_Results_sum,
                         SCM_Results_sum, SCF_Results_sum, IRF_Results_sum, IRM_Results_sum)

sum_results <- sum_results |> group_by(Country, Sex) |> mutate(sum_cont = sum(total_contribution)) |> ungroup()

abbrev_country_labs <- c("england_wales" = "E & W", "ireland" = "ROI",
                                         "northern_irl" = "NIR", "scotland" = "SCO")
sum_results %>% ggplot(aes(x=Country, y=total_contribution, fill=Cause))+
  facet_wrap(~Sex) +
  ggtitle("Total contributions to change in years lived between 0 and 75, 
          2007 to 2022")+ labs(x = "Country", y = "Total contribution") + 
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack")  +  theme(plot.title = element_markdown(), legend.position="bottom") +theme_minimal() +
  scale_x_discrete(labels = abbrev_country_labs)

###COMBINE SEXES IN PLOTS
##with nice age labs

age_breaks <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40,
                45, 50, 55, 60, 65, 70)

age_labels <- c("0", "1–4", "5–9", "10–14", "15–19",
                "20–24", "25–29", "30–34", "35–39",
                "40–44", "45–49", "50–54", "55–59",
                "60–64", "65–69", "70–74")

library(ggpubr)

combined_plotM <- ggarrange(EW_M,IR_M,NI_M, SC_M, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
combined_plotF <- ggarrange(EW_F,IR_F,NI_F, SC_F, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


final_plotM <- annotate_figure(
  combined_plotM,
  top = text_grob(
    "Decomposition of change in years lived between 0 and 75 years. Males, 2007–2022",
    face = "bold",
    size = 16
  )
)

final_plotM

final_plotF <- annotate_figure(
  combined_plotF,
  top = text_grob(
    "Decomposition of change in years lived between 0 and 75 years. Females, 2007–2022",
    face = "bold",
    size = 16
  )
)

plots <- ggarrange(
  EW_M, IR_M, NI_M, SC_M,
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "none"
)


legend <- get_legend(
  EW_M + theme(legend.position = "bottom")
)

final_plotM <- ggarrange(
  annotate_figure(
    plots,
    top = text_grob(
      "Decomposition of change in years lived between 0 and 75 years. 
      Males, 2007–2022",
      face = "bold",
      size = 16
    ),
    left = text_grob(
      "Contribution to change in years lived",
      rot = 90,
      size = 12
    ),
    bottom = text_grob(
      "Age at start of interval",
      size = 12
    )
  ),
  legend,
  ncol = 1,
  heights = c(1, 0.08)
)

final_plotM

plotsF <- ggarrange(
  EW_F, IR_F, NI_F, SC_F,
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "none"
)

legendF <- get_legend(
  EW_F + theme(legend.position = "bottom")
)

final_plotF <- ggarrange(
  annotate_figure(
    plotsF,
    top = text_grob(
      "Decomposition of change in years lived between 0 and 75 years. 
      Females, 2007–2022",
      face = "bold",
      size = 16
    ),
    left = text_grob(
      "Contribution to change in years lived",
      rot = 90,
      size = 12
    ),
    bottom = text_grob(
      "Age at start of interval",
      size = 12
    )
  ),
  legend,
  ncol = 1,
  heights = c(1, 0.08)
)

final_plotF


######need to know totals by age group for each country

EWF_Results_age <- EWF_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
EWF_Results_age <- EWF_Results_age |> distinct(Age, total)
EWM_Results_age <- EWM_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
EWM_Results_age <- EWM_Results_age |> distinct(Age, total)

IRF_Results_age <- IRF_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
IRF_Results_age <- IRF_Results_age |> distinct(Age, total)
IRM_Results_age <- IRM_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
IRM_Results_age <- IRM_Results_age |> distinct(Age, total)

NIF_Results_age <- NIF_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
NIF_Results_age <- NIF_Results_age |> distinct(Age, total)
NIM_Results_age <- NIM_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
NIM_Results_age <- NIM_Results_age |> distinct(Age, total)

SCF_Results_age <- SCF_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
SCF_Results_age <- SCF_Results_age |> distinct(Age, total)
SCM_Results_age <- SCM_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
SCM_Results_age <- SCM_Results_age |> distinct(Age, total)

######what about contributions by cancer for each age and country
EWF_results_ca <- EWF_Results |> filter(Cause %in% c("50/50 cancer", "not avoidable cancer", "treatable cancer", "preventable cancer")) |> 
  group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution)) |> distinct(Age, total)
EWM_results_ca <- EWM_Results |> filter(Cause %in% c("50/50 cancer", "not avoidable cancer", "treatable cancer", "preventable cancer")) |> 
  group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution)) |> distinct(Age, total)

IRF_results_ca <- IRF_Results |> filter(Cause %in% c("50/50 cancer", "not avoidable cancer", "treatable cancer", "preventable cancer")) |> 
  group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution)) |> distinct(Age, total)
IRM_results_ca <- IRM_Results |> filter(Cause %in% c("50/50 cancer", "not avoidable cancer", "treatable cancer", "preventable cancer")) |> 
  group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution)) |> distinct(Age, total)

NIF_results_ca <- NIF_Results |> filter(Cause %in% c("50/50 cancer", "not avoidable cancer", "treatable cancer", "preventable cancer")) |> 
  group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution)) |> distinct(Age, total)
NIM_results_ca <- NIM_Results |> filter(Cause %in% c("50/50 cancer", "not avoidable cancer", "treatable cancer", "preventable cancer")) |> 
  group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution)) |> distinct(Age, total)

SCF_results_ca <- SCF_Results |> filter(Cause %in% c("50/50 cancer", "not avoidable cancer", "treatable cancer", "preventable cancer")) |> 
  group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution)) |> distinct(Age, total)
SCM_results_ca <- SCM_Results |> filter(Cause %in% c("50/50 cancer", "not avoidable cancer", "treatable cancer", "preventable cancer")) |> 
  group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution)) |> distinct(Age, total)
