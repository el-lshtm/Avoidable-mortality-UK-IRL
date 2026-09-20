#####sensitivity analysis - comaprison of difference between 07 and 19, quantify effect of covid-19

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

##this has data for 2007 and 2022, can use for 2007 numbers
NEWage_causemx <- import(file = "data/NEWage_causemx.csv")
age_causemx07 <- NEWage_causemx |> filter(Year == "2007")
age_causemx07 <- age_causemx07 |> arrange(age_group)
age_causemx19 <- import(file = "data/2019age_causemx.csv") |> subset(select = -c(prop.t, prop.t_pyramid))
age_causemx <- bind_rows(age_causemx07, age_causemx19)


##reformat for decomp

age_causemx_wide <- age_causemx %>% 
  pivot_wider(
    names_from = fill_group,
    values_from = age_causemx
  ) %>% clean_names() %>% replace(is.na(.), 0)

age_causemx_wide <- age_causemx_wide %>%
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
age_causemx_wide <- age_causemx_wide %>% arrange(age_group, country, sex, year)

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

EW_men <- age_causemx_wide %>% filter(country=="england_wales", sex=="male")
EWM_COD07        <- as.matrix(EW_men[EW_men$year==2007,1:8])
EWM_COD19       <- as.matrix(EW_men[EW_men$year==2019,1:8])
EWM_Results <- horiuchi(func = PY075, pars1 = c(EWM_COD07), pars2 = c(EWM_COD19), N = 100)
dim(EWM_Results) <- dim(EWM_COD07)

##makes the decomp results into a df that can be plotted
EWM_Results            <- data.frame(EWM_Results)
colnames(EWM_Results)  <- cause_names
EWM_Results$Age        <- c(0,1,seq(5,85,5))
rownames(EWM_Results)  <- age_names
EWM_Results            <- gather(data = EWM_Results,key = Cause,value = Contribution,-Age) 
EWM_Results <- dplyr::filter(EWM_Results, !Age %in% c(75, 80, 85))

##calc total contribution by cause
EWM_Results_sum <- EWM_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "england_wales", Sex = "male")

##for ew females
EW_women <- age_causemx_wide %>% filter(country=="england_wales", sex=="female")
EWF_COD07        <- as.matrix(EW_women[EW_women$year==2007,1:8])
EWF_COD19       <- as.matrix(EW_women[EW_women$year==2019,1:8])
EWF_Results <- horiuchi(func = PY075F, pars1 = c(EWF_COD07), pars2 = c(EWF_COD19), N = 100)
dim(EWF_Results) <- dim(EWF_COD07)

##makes the decomp results into a df that can be plotted
EWF_Results            <- data.frame(EWF_Results)
colnames(EWF_Results)  <- cause_names
EWF_Results$Age        <- c(0,1,seq(5,85,5))
rownames(EWF_Results)  <- age_names
EWF_Results            <- gather(data = EWF_Results,key = Cause,value = Contribution,-Age) 
EWF_Results <- dplyr::filter(EWF_Results, !Age %in% c(75, 80, 85))

##calc total contribution by cause
EWF_Results_sum <- EWF_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "england_wales", Sex = "female")

#######IRL
IR_men <- age_causemx_wide %>% filter(country=="ireland", sex=="male")
IRM_COD07        <- as.matrix(IR_men[IR_men$year==2007,1:8])
IRM_COD19       <- as.matrix(IR_men[IR_men$year==2019,1:8])
IRM_Results <- horiuchi(func = PY075, pars1 = c(IRM_COD07), pars2 = c(IRM_COD19), N = 100)
dim(IRM_Results) <- dim(IRM_COD07)

##makes the decomp results into a df that can be plotted
IRM_Results            <- data.frame(IRM_Results)
colnames(IRM_Results)  <- cause_names
IRM_Results$Age        <- c(0,1,seq(5,85,5))
rownames(IRM_Results)  <- age_names
IRM_Results            <- gather(data = IRM_Results,key = Cause,value = Contribution,-Age) 
IRM_Results <- dplyr::filter(IRM_Results, !Age %in% c(75, 80, 85))


##calc total contribution by cause
IRM_Results_sum <- IRM_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "ireland", Sex = "male")

##for IR females
IR_women <- age_causemx_wide %>% filter(country=="ireland", sex=="female")
IRF_COD07        <- as.matrix(IR_women[IR_women$year==2007,1:8])
IRF_COD19       <- as.matrix(IR_women[IR_women$year==2019,1:8])
IRF_Results <- horiuchi(func = PY075F, pars1 = c(IRF_COD07), pars2 = c(IRF_COD19), N = 100)
dim(IRF_Results) <- dim(IRF_COD07)

##makes the decomp results into a df that can be plotted
IRF_Results            <- data.frame(IRF_Results)
colnames(IRF_Results)  <- cause_names
IRF_Results$Age        <- c(0,1,seq(5,85,5))
rownames(IRF_Results)  <- age_names
IRF_Results            <- gather(data = IRF_Results,key = Cause,value = Contribution,-Age) 
IRF_Results <- dplyr::filter(IRF_Results, !Age %in% c(75, 80, 85))

##calc total contribution by cause
IRF_Results_sum <- IRF_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "ireland", Sex = "female")

##########for sco
SC_men <- age_causemx_wide %>% filter(country=="scotland", sex=="male")
SCM_COD07        <- as.matrix(SC_men[SC_men$year==2007,1:8])
SCM_COD19       <- as.matrix(SC_men[SC_men$year==2019,1:8])
SCM_Results <- horiuchi(func = PY075, pars1 = c(SCM_COD07), pars2 = c(SCM_COD19), N = 100)
dim(SCM_Results) <- dim(SCM_COD07)

##makes the decomp results into a df that can be plotted
SCM_Results            <- data.frame(SCM_Results)
colnames(SCM_Results)  <- cause_names
SCM_Results$Age        <- c(0,1,seq(5,85,5))
rownames(SCM_Results)  <- age_names
SCM_Results            <- gather(data = SCM_Results,key = Cause,value = Contribution,-Age) 
SCM_Results <- dplyr::filter(SCM_Results, !Age %in% c(75, 80, 85))

##calc total contribution by cause
SCM_Results_sum <- SCM_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "scotland", Sex = "male")

##for IR females
SC_women <- age_causemx_wide %>% filter(country=="scotland", sex=="female")
SCF_COD07        <- as.matrix(SC_women[SC_women$year==2007,1:8])
SCF_COD19       <- as.matrix(SC_women[SC_women$year==2019,1:8])
SCF_Results <- horiuchi(func = PY075F, pars1 = c(SCF_COD07), pars2 = c(SCF_COD19), N = 100)
dim(SCF_Results) <- dim(SCF_COD07)

##makes the decomp results into a df that can be plotted
SCF_Results            <- data.frame(SCF_Results)
colnames(SCF_Results)  <- cause_names
SCF_Results$Age        <- c(0,1,seq(5,85,5))
rownames(SCF_Results)  <- age_names
SCF_Results            <- gather(data = SCF_Results,key = Cause,value = Contribution,-Age) 
SCF_Results <- dplyr::filter(SCF_Results, !Age %in% c(75, 80, 85))


##calc total contribution by cause
SCF_Results_sum <- SCF_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "scotland", Sex = "female")


#######and nir
NI_men <- age_causemx_wide %>% filter(country=="northern_irl", sex=="male")
NIM_COD07        <- as.matrix(NI_men[NI_men$year==2007,1:8])
NIM_COD19       <- as.matrix(NI_men[NI_men$year==2019,1:8])
NIM_Results <- horiuchi(func = PY075, pars1 = c(NIM_COD07), pars2 = c(NIM_COD19), N = 100)
dim(NIM_Results) <- dim(NIM_COD07)

##makes the decomp results into a df that can be plotted
NIM_Results            <- data.frame(NIM_Results)
colnames(NIM_Results)  <- cause_names
NIM_Results$Age        <- c(0,1,seq(5,85,5))
rownames(NIM_Results)  <- age_names
NIM_Results            <- gather(data = NIM_Results,key = Cause,value = Contribution,-Age) 
NIM_Results <- dplyr::filter(NIM_Results, !Age %in% c(75, 80, 85))


##calc total contribution by cause
NIM_Results_sum <- NIM_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "northern_irl", Sex = "male")

##for IR females
NI_women <- age_causemx_wide %>% filter(country=="northern_irl", sex=="female")
NIF_COD07        <- as.matrix(NI_women[NI_women$year==2007,1:8])
NIF_COD19       <- as.matrix(NI_women[NI_women$year==2019,1:8])
NIF_Results <- horiuchi(func = PY075F, pars1 = c(NIF_COD07), pars2 = c(NIF_COD19), N = 100)
dim(NIF_Results) <- dim(NIF_COD07)

##makes the decomp results into a df that can be plotted
NIF_Results            <- data.frame(NIF_Results)
colnames(NIF_Results)  <- cause_names
NIF_Results$Age        <- c(0,1,seq(5,85,5))
rownames(NIF_Results)  <- age_names
NIF_Results            <- gather(data = NIF_Results,key = Cause,value = Contribution,-Age) 
NIF_Results <- dplyr::filter(NIF_Results, !Age %in% c(75, 80, 85))

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
          2007 to 2019")+ labs(x = "Country", y = "Total contribution") + 
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack")  +  theme(plot.title = element_markdown(), legend.position="bottom") +theme_minimal()+
  scale_x_discrete(labels = abbrev_country_labs)

options(scipen=999)

######### still adds up roughly correctly - but why such a change in not avoidable non cancer causes??
##look at individual plots
#now graph results
NI_F <- ggplot(data=NIF_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Northern Ireland")+ 
  scale_fill_paletteer_d("tvthemes::Alexandrite") +  labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()

EW_M <-ggplot(data=EWM_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("England & Wales") +
  scale_fill_paletteer_d("tvthemes::Alexandrite") + labs(x = NULL, y = NULL) +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.075, 0.2)) +  theme(plot.title = element_markdown()) +theme_minimal()





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
