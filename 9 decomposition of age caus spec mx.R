##9 decomposition of e0



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
myHMDusername <- "eleanor.lucas1@student.lshtm.ac.uk"
myHMDpassword <- "CYt$d3fR5_XQ8ih"
age_causemx <- import(file = "data/NEWage_causemx.csv")

##first need to make age and cause specific mort rates wide format

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

cause_names<-c("1"="50/50 cancer", "2"="50/50 other","3"="not avoidable cancer",
               "4"="not avoidable other","5"="preventable cancer",
               "6"= "preventable other","7"="treatable cancer",
               "8"="treatable other")
age_names<-c("0"="0","1"="1-4","5"="5-9","10"="10-14","15"="15-19","20"="20-24",
             "25"="25-29","30"="30-34","35"="35-39","40"="40-44","45"="45-49",
             "50"="50-54","55"="55-59","60"="60-64","65"="65-69","70"="70-74",
             "75"="75-79","80"="80-84","85"="85+")     


################################################################################################
##alt mx using 85 as open ended intevral
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

    
##try filtering to ew males to start off with
EW_men <- NEWage_causemx_wide %>% filter(country=="england_wales", sex=="male")

COD1        <- as.matrix(EW_men[EW_men$year==2007,1:8])

COD2        <- as.matrix(EW_men[EW_men$year==2022,1:8])




##function calculating e0 using 2 vectors of mx
e0.frommx <- function(nmx =  mx, sex=1, age = c(0, 1, seq(5, 85, 5)), nax = NULL){
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
  ex <- Tx/lx
  e0 <- ex[1]
  
  return(e0)
}

e0frommxc <- function(mxcvec,sex=1){
    dim(mxcvec) <- c(19,length(mxcvec)/19) #correct number of rows - 16 0 to 75__
    mx          <- rowSums(mxcvec)
    e0.frommx(mx,sex)
}

##also need the same funcitons when sex = 2? females
e0.frommx_f <- function(nmx =  mx, sex=2, age = c(0, 1, seq(5, 85, 5)), nax = NULL){
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
  ex <- Tx/lx
  e0 <- ex[1]
  
  return(e0)
}

e0frommxc_f <- function(mxcvec,sex=2){
  dim(mxcvec) <- c(19,length(mxcvec)/19)
  mx          <- rowSums(mxcvec)
  e0.frommx_f(mx,sex)
}
  
  
Results <- horiuchi(func = e0frommxc, pars1 = c(COD1), pars2 = c(COD2), N = 100)
  
dim(Results) <- dim(COD1)
Results



##makes the decomp results into a df that can be plotted
Results            <- data.frame(Results)
colnames(Results)  <- cause_names
Results$Age        <- c(0,1,seq(5,85,5))
rownames(Results)  <- age_names
Results            <- gather(data = Results,key = Cause,value = Contribution,-Age) 

#now graph results
ggplot(data=Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 males in E&W' ))+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.25, 0.75))

##calc total contribution by cause
Results_sum <- Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "england_wales", Sex = "male")


##compare with females?
EW_women <- NEWage_causemx_wide %>% filter(country=="england_wales", sex=="female")
EWf_COD07        <- as.matrix(EW_women[EW_women$year==2007,1:8])

EWf_COD22       <- as.matrix(EW_women[EW_women$year==2022,1:8])

EWf_Results <- horiuchi(func = e0frommxc_f, pars1 = c(EWf_COD07), pars2 = c(EWf_COD22), N = 100)
dim(EWf_Results) <- dim(EWf_COD07)

EWf_Results            <- data.frame(EWf_Results)
colnames(EWf_Results)  <- cause_names
EWf_Results$Age        <- c(0,1,seq(5,85,5))
rownames(EWf_Results)  <- age_names
EWf_Results            <- gather(data = EWf_Results,key = Cause,value = Contribution,-Age) 

#now graph results
ggplot(data=EWf_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 females in E&W' ))+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack")  + coord_cartesian(ylim = c(-0.25, 0.75))

EWf_Results_sum <- EWf_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "england_wales", Sex = "female")


##irl probably going to show the most difference - look at that next
IR_women <- NEWage_causemx_wide %>% filter(country=="ireland", sex=="female")
IRf_COD07        <- as.matrix(IR_women[IR_women$year==2007,1:8])

IRf_COD22       <- as.matrix(IR_women[IR_women$year==2022,1:8])

IRf_Results <- horiuchi(func = e0frommxc_f, pars1 = c(IRf_COD07), pars2 = c(IRf_COD22), N = 100)
dim(IRf_Results) <- dim(IRf_COD07)

IRf_Results            <- data.frame(IRf_Results)
colnames(IRf_Results)  <- cause_names
IRf_Results$Age        <- c(0,1,seq(5,85,5))
rownames(IRf_Results)  <- age_names
IRf_Results            <- gather(data = IRf_Results,key = Cause,value = Contribution,-Age) 

#now graph results
ggplot(data=IRf_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 females in Ireland' ))+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.25, 0.75))

IRf_Results_sum <- IRf_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "ireland", Sex = "female")

#for males
IR_men <- NEWage_causemx_wide %>% filter(country=="ireland", sex=="male")
IRm_COD07        <- as.matrix(IR_men[IR_men$year==2007,1:8])

IRm_COD22       <- as.matrix(IR_men[IR_men$year==2022,1:8])

IRm_Results <- horiuchi(func = e0frommxc, pars1 = c(IRm_COD07), pars2 = c(IRm_COD22), N = 100)
dim(IRm_Results) <- dim(IRm_COD07)

IRm_Results            <- data.frame(IRm_Results)
colnames(IRm_Results)  <- cause_names
IRm_Results$Age        <- c(0,1,seq(5,85,5))
rownames(IRm_Results)  <- age_names
IRm_Results            <- gather(data = IRm_Results,key = Cause,value = Contribution,-Age) 

#now graph results
ggplot(data=IRm_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 males in Ireland' ))+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.25, 0.75))

IRm_Results_sum <- IRm_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "ireland", Sex = "male")


##now for scotland
SC_women <- NEWage_causemx_wide %>% filter(country=="scotland", sex=="female")
SCf_COD07        <- as.matrix(SC_women[SC_women$year==2007,1:8])

SCf_COD22       <- as.matrix(SC_women[SC_women$year==2022,1:8])

SCf_Results <- horiuchi(func = e0frommxc_f, pars1 = c(SCf_COD07), pars2 = c(SCf_COD22), N = 100)
dim(SCf_Results) <- dim(SCf_COD07)

SCf_Results            <- data.frame(SCf_Results)
colnames(SCf_Results)  <- cause_names
SCf_Results$Age        <- c(0,1,seq(5,85,5))
rownames(SCf_Results)  <- age_names
SCf_Results            <- gather(data = SCf_Results,key = Cause,value = Contribution,-Age) 

#now graph results
ggplot(data=SCf_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 females in Scotland' ))+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack")  + coord_cartesian(ylim = c(-0.25, 0.75))

SCf_Results_sum <- SCf_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "scotland", Sex = "female")

#for males
SC_men <- NEWage_causemx_wide %>% filter(country=="scotland", sex=="male")
SCm_COD07        <- as.matrix(SC_men[SC_men$year==2007,1:8])

SCm_COD22       <- as.matrix(SC_men[SC_men$year==2022,1:8])

SCm_Results <- horiuchi(func = e0frommxc, pars1 = c(SCm_COD07), pars2 = c(SCm_COD22), N = 100)
dim(SCm_Results) <- dim(SCm_COD07)

SCm_Results            <- data.frame(SCm_Results)
colnames(SCm_Results)  <- cause_names
SCm_Results$Age        <- c(0,1,seq(5,85,5))
rownames(SCm_Results)  <- age_names
SCm_Results            <- gather(data = SCm_Results,key = Cause,value = Contribution,-Age) 

#now graph results
ggplot(data=SCm_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 males in Scotland' ))+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack")  + coord_cartesian(ylim = c(-0.25, 0.75))

SCm_Results_sum <- SCm_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "scotland", Sex = "male")


##AND FOR NIR
NIR_women <- NEWage_causemx_wide %>% filter(country=="northern_irl", sex=="female")
NIRf_COD07        <- as.matrix(NIR_women[NIR_women$year==2007,1:8])

NIRf_COD22       <- as.matrix(NIR_women[NIR_women$year==2022,1:8])

NIRf_Results <- horiuchi(func = e0frommxc_f, pars1 = c(NIRf_COD07), pars2 = c(NIRf_COD22), N = 100)
dim(NIRf_Results) <- dim(NIRf_COD07)

NIRf_Results            <- data.frame(NIRf_Results)
colnames(NIRf_Results)  <- cause_names
NIRf_Results$Age        <- c(0,1,seq(5,85,5))
rownames(NIRf_Results)  <- age_names
NIRf_Results            <- gather(data = NIRf_Results,key = Cause,value = Contribution,-Age) 

#now graph results
ggplot(data=NIRf_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 females in Northern Ireland' ))+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.25, 0.75))

NIRf_Results_sum <- NIRf_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "northern_irl", Sex = "female")

#for males
NIR_men <- NEWage_causemx_wide %>% filter(country=="northern_irl", sex=="male")
NIRm_COD07        <- as.matrix(NIR_men[NIR_men$year==2007,1:8])

NIRm_COD22       <- as.matrix(NIR_men[IR_men$year==2022,1:8])

NIRm_Results <- horiuchi(func = e0frommxc, pars1 = c(NIRm_COD07), pars2 = c(NIRm_COD22), N = 100)
dim(NIRm_Results) <- dim(NIRm_COD07)

NIRm_Results            <- data.frame(NIRm_Results)
colnames(NIRm_Results)  <- cause_names
NIRm_Results$Age        <- c(0,1,seq(5,85,5))
rownames(NIRm_Results)  <- age_names
NIRm_Results            <- gather(data = NIRm_Results,key = Cause,value = Contribution,-Age) 

#now graph results
ggplot(data=NIRm_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 males in Northern Ireland' ))+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.25, 0.75))

NIRm_Results_sum <- NIRm_Results %>% group_by(Cause) %>% 
  mutate(total_contribution = sum(Contribution)) %>% distinct(total_contribution) %>% mutate(Country = "northern_irl", Sex = "male")


###plotting decomp next to each for comparison
EW_m <- ggplot(data=Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("England & Wales")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.1, 0.55)) +  
  theme(plot.title = element_markdown()) +theme_minimal() +  labs(x = NULL, y = NULL)
EW_f <- ggplot(data=EWf_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("England & Wales")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.1, 0.55)) +  
  theme(plot.title = element_markdown()) +theme_minimal() +  labs(x = NULL, y = NULL)
SC_m <- ggplot(data=SCm_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Scotland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.1, 0.55)) + 
  theme(plot.title = element_markdown()) +theme_minimal() +  labs(x = NULL, y = NULL)
SC_f <- ggplot(data=SCf_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Scotland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.1, 0.55)) + 
  theme(plot.title = element_markdown()) +theme_minimal() +  labs(x = NULL, y = NULL)
IR_m <- ggplot(data=IRm_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Ireland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.1, 0.55)) + 
  theme(plot.title = element_markdown()) +theme_minimal() +  labs(x = NULL, y = NULL)
IR_f <- ggplot(data=IRf_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Ireland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.1, 0.55)) + 
  theme(plot.title = element_markdown()) +theme_minimal() +  labs(x = NULL, y = NULL)
NIR_m <- ggplot(data=NIRm_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Northern Ireland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.1, 0.55)) + 
  theme(plot.title = element_markdown()) +theme_minimal() +  labs(x = NULL, y = NULL)
NIR_f <- ggplot(data=NIRf_Results, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle("Northern Ireland")+
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack") + coord_cartesian(ylim = c(-0.1, 0.55)) + 
  theme(plot.title = element_markdown()) +theme_minimal() +  labs(x = NULL, y = NULL)

library(ggpubr)

###COMBINE SEXES IN PLOTS
##with nice age labs

combined_plotM <- ggarrange(EW_m,IR_m,NIR_m, SC_m, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
combined_plotF <- ggarrange(EW_f,IR_f,NIR_f, SC_f, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


final_plotM <- annotate_figure(
  combined_plotM,
  top = text_grob(
    "Decomposition of change in life expectancy at birth. Males, 2007–2022",
    face = "bold",
    size = 16
  )
)

final_plotM

final_plotF <- annotate_figure(
  combined_plotF,
  top = text_grob(
    "Decomposition of change in life expectancy at birth. Females, 2007–2022",
    face = "bold",
    size = 16
  )
)

plots <- ggarrange(
  EW_m, IR_m, NIR_m, SC_m,
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "none"
)


legend <- get_legend(
  EW_m + theme(legend.position = "bottom")
)

final_plotM <- ggarrange(
  annotate_figure(
    plots,
    top = text_grob(
      "Decomposition of change in life expectancy at birth. 
      Males, 2007–2022",
      face = "bold",
      size = 16
    ),
    left = text_grob(
      "Contribution to change in LE",
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
  EW_f, IR_f, NIR_f, SC_f,
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "none"
)

legendF <- get_legend(
  EW_f + theme(legend.position = "bottom")
)

final_plotF <- ggarrange(
  annotate_figure(
    plotsF,
    top = text_grob(
      "Decomposition of change in life expectancy at birth. 
      Females, 2007–2022",
      face = "bold",
      size = 16
    ),
    left = text_grob(
      "Contribution to change in LE",
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


##combine summed results

sum_results <- bind_rows(NIRf_Results_sum, NIRm_Results_sum, Results_sum, EWf_Results_sum,
                         SCm_Results_sum, SCf_Results_sum, IRf_Results_sum, IRm_Results_sum)

sum_results <- sum_results |> group_by(Country, Sex) |> mutate(sum_cont = sum(total_contribution)) |> ungroup()

abbrev_country_labs <- c("england_wales" = "E & W", "ireland" = "ROI",
                         "northern_irl" = "NIR", "scotland" = "SCO")
sum_results %>% ggplot(aes(x=Country, y=total_contribution, fill=Cause))+
  facet_wrap(~Sex) +
  ggtitle("Total contributions to change in life expectancy at birth, 
          2007 to 2022")+ labs(x = "Country", y = "Total contribution") + 
  scale_fill_paletteer_d("tvthemes::Alexandrite") +
  geom_bar(stat = "identity", position = "stack")  +  theme(plot.title = element_markdown(), legend.position="bottom") +theme_minimal() +
  scale_x_discrete(labels = abbrev_country_labs)



######need to know totals by age group for each country

EWf_Results_age <- EWf_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
EWf_Results_age <- EWf_Results_age |> distinct(Age, total)
EWm_Results_age <- Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
EWm_Results_age <- EWm_Results_age |> distinct(Age, total)

IRf_Results_age <- IRf_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
IRf_Results_age <- IRf_Results_age |> distinct(Age, total)
IRm_Results_age <- IRm_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
IRm_Results_age <- IRm_Results_age |> distinct(Age, total)

NIRf_Results_age <- NIRf_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
NIRf_Results_age <- NIRf_Results_age |> distinct(Age, total)
NIRm_Results_age <- NIRm_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
NIRm_Results_age <- NIRm_Results_age |> distinct(Age, total)

SCf_Results_age <- SCf_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
SCf_Results_age <- SCf_Results_age |> distinct(Age, total)
SCm_Results_age <- SCm_Results |> group_by(Age) |> mutate(total = sum(Contribution)) |> ungroup() |> subset(select = -c(Contribution))
SCm_Results_age <- SCm_Results_age |> distinct(Age, total)