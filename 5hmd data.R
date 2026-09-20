##looking at hmd data

rm(list = ls())

setwd("C:/Users/gleno/OneDrive/Documents/ELLIE UNI/PROJECT")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)
library(HMDHFDplus)

myHMDusername <- "insert-user"
myHMDpassword <- "insert-password"

#importing m and f lifetables for each country then joining them

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
LT_IRL <- left_join(FLT_IRL, MLT_IRL, join_by(Year==Year, Age==Age, OpenInterval==OpenInterval),
                    suffix = c(".f", ".m"))

pop_IRL <- readHMDweb(
  CNTRY = "IRL",
  item = "Population5",
  username = myHMDusername,
  password = myHMDpassword
)

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
LT_ENG_WA <- left_join(FLT_ENG_WA, MLT_ENG_WA, join_by(Year==Year, Age==Age, OpenInterval==OpenInterval),
                    suffix = c(".f", ".m"))

pop_ENG_WA <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "Population5",
  username = myHMDusername,
  password = myHMDpassword
)

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
LT_SCO <- left_join(FLT_SCO, MLT_SCO, join_by(Year==Year, Age==Age, OpenInterval==OpenInterval),
                       suffix = c(".f", ".m"))
pop_SCO <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "Population5",
  username = myHMDusername,
  password = myHMDpassword
)

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
LT_NIR <- left_join(FLT_NIR, MLT_NIR, join_by(Year==Year, Age==Age, OpenInterval==OpenInterval),
                    suffix = c(".f", ".m"))
pop_NIR <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "Population5",
  username = myHMDusername,
  password = myHMDpassword
)

##now m and f is join remove unjoined
rm(FLT_ENG_WA, FLT_IRL, FLT_NIR, FLT_SCO, MLT_ENG_WA, MLT_IRL, MLT_NIR, MLT_SCO)

LT_IRL07 <- LT_IRL %>% filter(Year==2007)
LT_IRL22 <- LT_IRL %>% filter(Year==2022)
pop_IRL07 <- pop_IRL %>% filter(Year==2007)
pop_IRL22 <- pop_IRL %>% filter(Year==2022)
LT_ENG_WA07 <-  LT_ENG_WA %>% filter(Year==2007)
LT_ENG_WA22 <-  LT_ENG_WA %>% filter(Year==2022)
pop_ENG_WA07 <- pop_ENG_WA %>% filter(Year==2007)
pop_ENG_WA22 <- pop_ENG_WA %>% filter(Year==2022)
LT_SCO07 <- LT_SCO %>% filter(Year==2007)
LT_SCO22 <- LT_SCO %>% filter(Year==2022)
pop_SCO07 <- pop_SCO %>% filter(Year==2007)
pop_SCO22 <- pop_SCO %>% filter(Year==2022)
LT_NIR07 <- LT_NIR %>% filter(Year==2007)
LT_NIR22 <-  LT_NIR %>% filter(Year==2022)
pop_NIR07 <- pop_NIR %>% filter(Year==2007)
pop_NIR22 <- pop_NIR %>% filter(Year==2022)

##now can remove large lts with all years
rm(LT_ENG_WA, LT_IRL, LT_NIR, LT_SCO, pop_NIR, pop_IRL, pop_ENG_WA, pop_SCO)


#calculating mid year pops 
pop_IRL07 <- pop_IRL07 %>% mutate(
  Female.mid = (Female1 + Female2)/2,
  Male.mid = (Male1 + Male2)/2
) 
pop_IRL22 <- pop_IRL22 %>% mutate(
  Female.mid = (Female1 + Female2)/2,
  Male.mid = (Male1 + Male2)/2
) 
pop_ENG_WA07 <- pop_ENG_WA07 %>% mutate(
  Female.mid = (Female1 + Female2)/2,
  Male.mid = (Male1 + Male2)/2
) 
pop_ENG_WA22 <- pop_ENG_WA22 %>% mutate(
  Female.mid = (Female1 + Female2)/2,
  Male.mid = (Male1 + Male2)/2
) 
pop_SCO07 <- pop_SCO07 %>% mutate(
  Female.mid = (Female1 + Female2)/2,
  Male.mid = (Male1 + Male2)/2
)
pop_SCO22 <- pop_SCO22 %>% mutate(
  Female.mid = (Female1 + Female2)/2,
  Male.mid = (Male1 + Male2)/2
)
pop_NIR22 <- pop_NIR22 %>% mutate(
  Female.mid = (Female1 + Female2)/2,
  Male.mid = (Male1 + Male2)/2
)
pop_NIR07 <- pop_NIR07 %>% mutate(
  Female.mid = (Female1 + Female2)/2,
  Male.mid = (Male1 + Male2)/2
)
head(pop_ENG_WA07)


# Reshape to long format for plotting
##make 85 plus the open ended interval
##deaths from hmd as solid bars, op as transparent bars? far too small compared to total pop

pop_ENG_WA22 <- pop_ENG_WA22 %>%
  select(Year, Age, OpenInterval, Female.mid, Male.mid) %>%
  mutate(
      Age = ifelse(OpenInterval == TRUE | Age >= 85, "85+", as.character(Age)),
      Age = factor(Age, levels = c(as.character(0:84), "85+"))
    ) %>%
  group_by(Year, Age) %>%
  summarise(Female.mid = sum(Female.mid),
            Male.mid = sum(Male.mid),
            .groups = "drop") %>%
  pivot_longer(cols = c(Female.mid, Male.mid),
               names_to = "Sex",
               values_to = "Population") %>%
  mutate(
    Sex = ifelse(Sex == "Female.mid", "Female", "Male"),
    Population = ifelse(Sex == "Female", -Population, Population)
  )

ggplot(pop_ENG_WA22, aes(x = Population, y = factor(Age), fill = Sex)) +
  geom_col() +
  scale_x_continuous(
    labels = function(x) scales::comma(abs(x)),  # show positive numbers on both sides
    name = "Population"
  ) +
  scale_fill_manual(values = c("Female" = "#E07B7B", "Male" = "#5B8DB8")) +
  labs(
    title = "Population Pyramid: England & Wales 2022",
    y = "Age",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  )

deaths_EW <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "Deaths_5x1",
  username = myHMDusername,
  password = myHMDpassword
)
deaths_EW22 <- deaths_EW %>% filter(Year==2022)
head(deaths_EW22)

deaths_long <- deaths_EW22 %>%
  select(Year, Age, OpenInterval, Female, Male) %>%
  mutate(
    Age = ifelse(OpenInterval == TRUE | Age >= 85, "85+", as.character(Age)),
    Age = factor(Age, levels = c(as.character(0:84), "85+"))
  ) %>%
  group_by(Year, Age) %>%
  summarise(Female = sum(Female),
            Male = sum(Male),
            .groups = "drop") %>%
  pivot_longer(cols = c(Female, Male),
               names_to = "Sex",
               values_to = "Deaths") %>%
  mutate(
    Deaths = ifelse(Sex == "Female", -Deaths, Deaths)
  )

# Join and plot
left_join(pop_ENG_WA22, deaths_long, by = c("Year", "Age", "Sex")) %>%
  filter(!Age %in% c("75", "80", "85+")) %>% 
  ggplot(aes(y = Age)) +
  geom_col(aes(x = Population, fill = Sex), alpha = 0.6) +
  geom_col(aes(x = Deaths, fill = Sex), alpha = 1) +
  scale_x_continuous(
    labels = function(x) scales::comma(abs(x)),
    name = "Population / Deaths"
  ) +
  scale_fill_manual(values = c("Female" = "#E07B7B", "Male" = "#5B8DB8")) +
  labs(
    title = "Population and Deaths: England & Wales",
    y = "Age",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  )

##maybe would look better in less populous countries?

pop_IRL22 <- pop_IRL22 %>%
  select(Year, Age, OpenInterval, Female.mid, Male.mid) %>%
  mutate(
    Age = ifelse(OpenInterval == TRUE | Age >= 85, "85+", as.character(Age)),
    Age = factor(Age, levels = c(as.character(0:84), "85+"))
  ) %>%
  group_by(Year, Age) %>%
  summarise(Female.mid = sum(Female.mid),
            Male.mid = sum(Male.mid),
            .groups = "drop") %>%
  pivot_longer(cols = c(Female.mid, Male.mid),
               names_to = "Sex",
               values_to = "Population") %>%
  mutate(
    Sex = ifelse(Sex == "Female.mid", "Female", "Male"),
    Population = ifelse(Sex == "Female", -Population, Population)
  )

ggplot(pop_IRL22, aes(x = Population, y = factor(Age), fill = Sex)) +
  geom_col() +
  scale_x_continuous(
    labels = function(x) scales::comma(abs(x)),  # show positive numbers on both sides
    name = "Population"
  ) +
  scale_fill_manual(values = c("Female" = "#E07B7B", "Male" = "#5B8DB8")) +
  labs(
    title = "Population Pyramid: IRELAND 2022",
    y = "Age",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  )

deaths_IR <- readHMDweb(
  CNTRY = "IRL",
  item = "Deaths_5x1",
  username = myHMDusername,
  password = myHMDpassword
)
deaths_IR22 <- deaths_IR %>% filter(Year==2022)


deaths_long_IR22 <- deaths_IR22 %>%
  select(Year, Age, OpenInterval, Female, Male) %>%
  mutate(
    Age = ifelse(OpenInterval == TRUE | Age >= 85, "85+", as.character(Age)),
    Age = factor(Age, levels = c(as.character(0:84), "85+"))
  ) %>%
  group_by(Year, Age) %>%
  summarise(Female = sum(Female),
            Male = sum(Male),
            .groups = "drop") %>%
  pivot_longer(cols = c(Female, Male),
               names_to = "Sex",
               values_to = "Deaths") %>%
  mutate(
    Deaths = ifelse(Sex == "Female", -Deaths, Deaths)
  )

# Join and plot
left_join(pop_IRL22, deaths_long_IR22, by = c("Year", "Age", "Sex")) %>%
  filter(!Age %in% c("75", "80", "85+")) %>% 
  ggplot(aes(y = Age)) +
  geom_col(aes(x = Population, fill = Sex), alpha = 0.6) +
  geom_col(aes(x = Deaths, fill = Sex), alpha = 1) +
  scale_x_continuous(
    labels = function(x) scales::comma(abs(x)),
    name = "Population / Deaths"
  ) +
  scale_fill_manual(values = c("Female" = "#E07B7B", "Male" = "#5B8DB8")) +
  labs(
    title = "Population and Deaths: IRELAND 2022",
    y = "Age",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  )



#starting to look at decomp

##manual calculation for arriage decomposition
##cant use if there is no change in mort rates 
LT_ENG_WA07$ex.f[1] #female ex at birth is 81.81
LT_ENG_WA22$ex.f[1] #in 22 it is 83.07
big.delta <- LT_ENG_WA22$ex.f[1] - LT_ENG_WA07$ex.f[1] #1.26

l1<- LT_ENG_WA07$lx.f
l2<- LT_ENG_WA22$lx.f

L1<- LT_ENG_WA07$Lx.f
L2<- LT_ENG_WA22$Lx.f

T1<- LT_ENG_WA07$Tx.f
T2<- LT_ENG_WA22$Tx.f

LAG<- length(l1)

DE <- (l1/l1[1])*((L2/l2)-(L1/l1))

IE <- (T2[-1]/l1[1])*((l1[-LAG]/l2[-LAG])-(l1[-1]/l2[-1]))

# one extra value for the indirect component
# since there is only direct component in the last age group
IE<-c(IE,0)

ALL<-DE+IE
print(ALL)
sum(ALL) #equal 1.26, same as manual difference, slight difference in the two values

ggplot()+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 - 2022' ))+
  geom_bar(aes(x = factor(c(0,1,seq(5,110,5))), y= ALL),stat = "identity")+
  scale_x_discrete('Age group',guide = guide_axis(n.dodge = 2))
#shows most improveents in LE for females in EW are in ages 70 and above, slight negative contribution of age group 30 years

#compare with irish males
l1IR<- LT_IRL07$lx.m
l2IR<- LT_IRL22$lx.m

L1IR<- LT_IRL07$Lx.m
L2IR<- LT_IRL22$Lx.m

T1IR<- LT_IRL07$Tx.m
T2IR<- LT_IRL22$Tx.m

LAGIR<- length(l1)

DEIR <- (l1IR/l1IR[1])*((L2IR/l2IR)-(L1IR/l1IR))

IEIR <- (T2IR[-1]/l1IR[1])*((l1IR[-LAGIR]/l2IR[-LAGIR])-(l1IR[-1]/l2IR[-1]))

IEIR<-c(IEIR,0)

ALLIR<-DEIR+IEIR

sum(ALLIR)
ggplot()+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 - 2022 Irish males' ))+
  geom_bar(aes(x = factor(c(0,1,seq(5,110,5))), y= ALLIR),stat = "identity")+
  scale_x_discrete('Age group',guide = guide_axis(n.dodge = 2))
##shows more improvement in younger age groups, similar magnitude in contribution from these groups as the biggest contribution from
##any age group in EW women
#very little gains in age 0 for irish males

#how to do it with decomp package
library(DemoDecomp)
#need 2 vectors of rates
IR_mx1 <- LT_IRL07$mx.m
IR_mx2 <- LT_IRL22$mx.m

e0.frommx <- function(nmx =  mx, sex=1, age = c(0, 1, seq(5, 110, 5)), nax = NULL){
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

IR_Results <- horiuchi(func = e0.frommx, pars1 = IR_mx1, pars2 = IR_mx2,N = 100)
print(IR_Results)

ggplot()+
  ggtitle(bquote(~'Change in '~ e[0] ~'2007 to 2022 irish males horiuchi decomp' ))+
  geom_bar(aes(x = factor(c(0,1,seq(5,110,5))), y= Results), stat = "identity", position = "stack")


#doing the same for females in IR
IRf_mx1 <- LT_IRL07$mx.f
IRf_mx2 <- LT_IRL22$mx.f
IRf_Results <- horiuchi(func = e0.frommx, pars1 = IRf_mx1, pars2 = IRf_mx2,N = 100)
print(IRf_Results)


#doing the same for women in scotland - worst performing over time frame
SCf_mx1 <- LT_SCO07$mx.f
SCf_mx2 <- LT_SCO22$mx.f
SCf_Results <- horiuchi(func = e0.frommx, pars1 = SCf_mx1, pars2 = SCf_mx2,N = 100)
print(SCf_Results)
#negative contributions from middle adult ages

#for scottish men
SC_mx1 <- LT_SCO07$mx.m
SC_mx2 <- LT_SCO22$mx.m
SC_Results <- horiuchi(func = e0.frommx, pars1 = SC_mx1, pars2 = SC_mx2,N = 100)
print(SC_Results)

##for eng and w men and women

EW_mx1 <- LT_ENG_WA07$mx.m
EW_mx2 <- LT_ENG_WA22$mx.m
EW_Results <- horiuchi(func = e0.frommx, pars1 = EW_mx1, pars2 = EW_mx2,N = 100)

EWf_mx1 <- LT_ENG_WA07$mx.f
EWf_mx2 <- LT_ENG_WA22$mx.f
EWf_Results <- horiuchi(func = e0.frommx, pars1 = EWf_mx1, pars2 = EWf_mx2,N = 100)

##and for NIR

NIR_mx1 <- LT_NIR07$mx.m
NIR_mx2 <- LT_NIR22$mx.m
NIR_Results <- horiuchi(func = e0.frommx, pars1 = NIR_mx1, pars2 = NIR_mx2,N = 100)


NIRf_mx1 <- LT_NIR07$mx.f
NIRf_mx2 <- LT_NIR22$mx.f
NIRf_Results <- horiuchi(func = e0.frommx, pars1 = NIRf_mx1, pars2 = NIRf_mx2,N = 100)

ggplot()+
  geom_bar(aes(x = factor(c(0,1,seq(5,110,5))), y= NIRf_Results), stat = "identity", position = "stack") +
  labs(
    x = "Age group",
    y = "Contribution to change in e0",
    title = "Horiuchi decomposition of change in e0 for women in Northern Ireland: 2007 to 2022"
  ) +
  scale_x_discrete('Age group',guide = guide_axis(n.dodge = 2))



#how to get cause specific mort rates

#trying with EW 07 first
DC_EW07 <- combi_07_sex %>% filter(Country=="england_wales")
