####looking at adult life years lost using hmd lts
rm(list = ls())

setwd("C:/Users/gleno/OneDrive/Documents/ELLIE UNI/PROJECT")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)
library(gridExtra)
library(HMDHFDplus)

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
    PY_30_75 = (Tx[Age == 30] - Tx[Age == 75])/lx[Age ==30]
  ) %>% mutate(Sex = "female")
MLT_ENG_WA <- MLT_ENG_WA %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_30_75 = (Tx[Age == 30] - Tx[Age == 75])/lx[Age ==30]
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
    PY_30_75 = (Tx[Age == 30] - Tx[Age == 75])/lx[Age ==30]
  ) %>% mutate(Sex = "female")
MLT_IRL <- MLT_IRL %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_30_75 = (Tx[Age == 30] - Tx[Age == 75])/lx[Age ==30]
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
    PY_30_75 = (Tx[Age == 30] - Tx[Age == 75])/lx[Age ==30]
  ) %>% mutate(Sex = "female")
MLT_SCO <- MLT_SCO %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_30_75 = (Tx[Age == 30] - Tx[Age == 75])/lx[Age ==30]
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
    PY_30_75 = (Tx[Age == 30] - Tx[Age == 75])/lx[Age ==30]
  ) %>% mutate(Sex = "female")
MLT_NIR <- MLT_NIR %>% 
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), 
         !Age %in% c("80", "85", "90", "95", "100", "105", "110")) %>% 
  group_by(Year) %>% 
  summarise(
    PY_30_75 = (Tx[Age == 30] - Tx[Age == 75])/lx[Age ==30]
  ) %>% mutate(Sex = "male")
NIR <- bind_rows(FLT_NIR, MLT_NIR) %>% mutate(Country = "northern_irl")

all <- bind_rows(ENG_WA, SCO, IRL, NIR)


label_data <- all %>%
  filter(Year %in% c(2007, 2022))

all %>% ggplot(aes(x = Year, y = PY_30_75, color = Sex)) +
  geom_line(linewidth = 0.8) + coord_cartesian(ylim = c(40, 44)) + 
  scale_x_continuous(breaks = seq(2007, 2022, by = 2)) + geom_point() +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  scale_color_manual(name = "Sex", values = c("male" = "skyblue3", "female" = "violetred3")) +
  labs(
    x = "Year", 
    y = "Average years lived between 30 and 75",
    title = "Average number of years lived between 30 and 75 in the United Kingdom and Ireland, 2007 to 2022") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + geom_text(
    data = label_data,
    aes(label = round(PY_30_75, 1)),
    vjust = -0.7,
    size = 3
  )

##make a table for the two years of interest

library(gt)
label_data |> gt()

label_data %>% 
  gt(groupname_col = "Country") %>% 
  tab_header(
    title = "Average years lived between 30 and 75",
    subtitle = "2007 and 2022"
  ) |>
  cols_label(
    PY_30_75 = "Person-Years (30-75)",
    Sex = "Sex",
    Year = "Year"
  ) |>
  fmt_number(columns = PY_30_75, decimals = 3)
