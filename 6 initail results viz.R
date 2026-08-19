rm(list = ls())

setwd("C:/Users/gleno/OneDrive/Documents/ELLIE UNI/PROJECT")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)
library(gridExtra)
data2007 <- import(file = "data/avoidable_deaths_07.csv")
##average2007_09 <- import(file = "data/avoidable_deaths_07_09average.csv")
data2022 <- import(file = "data/avoidable_deaths_22.csv")
##average2020_22 <- import(file = "data/avoidable_deaths_20_22average.csv")
##options(scipen = 999)


##give the more sensible age group values - so it matches the hmd
data2007 <- data2007 %>% mutate(age_group = 
                                  recode(age_group, "<1" = "0", "1-4" = "1", "5-9" = "5", "10-14" = "10", "15-19" = "15", "20-24" = "20", "25-29" = "25",
                                         "30-34" = "30", "35-39" = "35", "40-44" = "40", "45-49" = "45", "50-54" = "50", "55-59" = "55", "60-64" = "60",
                                         "65-69" = "65", "70-74" = "70", "75-79" = "75", "80-84" = "80", ">85" = "85"))
data2022 <- data2022 %>% mutate(age_group = 
                                  recode(age_group, "<1" = "0", "1-4" = "1", "5-9" = "5", "10-14" = "10", "15-19" = "15", "20-24" = "20", "25-29" = "25",
                                         "30-34" = "30", "35-39" = "35", "40-44" = "40", "45-49" = "45", "50-54" = "50", "55-59" = "55", "60-64" = "60",
                                         "65-69" = "65", "70-74" = "70", "75-79" = "75", "80-84" = "80", ">85" = "85"))

####showing age structure of deaths for each country by sex
###deaths by age group as a proportion of total deaths
###visualise this first before separating by cause?

EW_2007 <- data2007 %>% filter(Country=="england_wales") %>% group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup()

##FEMALES values to negative to plot on either side
EW_2007 <- EW_2007 %>%
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) 
##reorder age group
EW_2007 <- EW_2007 %>%
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))


#now the same for 2022
EW_2022 <- data2022 %>% filter(Country=="england_wales") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))


##now the same for other countries for both years

IR_2022 <- data2022 %>% filter(Country=="ireland") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))
IR_2007 <- data2007 %>% filter(Country=="ireland") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

NIR_2022 <- data2022 %>% filter(Country=="northern_irl") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))
NIR_2007 <- data2007 %>% filter(Country=="northern_irl") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

SCO_2022 <- data2022 %>% filter(Country=="scotland") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))
SCO_2007 <- data2007 %>% filter(Country=="scotland") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

##making country labels
country_labs <- c("england_wales" = "England & Wales", "ireland" = "Republic of Ireland",
                  "northern_irl" = "Northern Ireland", "scotland" = "Scotland")

allprop_2007 <- bind_rows(SCO_2007, EW_2007, IR_2007, NIR_2007)
allprop_2022 <- bind_rows(SCO_2022, EW_2022, IR_2022, NIR_2022)

##########visualising this data as a sort of table 1 - for appendix of report?
all_07_wide <- allprop_2007 |> subset(select = -c(avoidable, total_deaths.o, proportion.o, total_deaths.c, proportion.c,
                                            proportion.b, death_type, deaths_pyramid, prop.t, prop.t_pyramid)) |> 
  pivot_wider(names_from = fill_group, values_from = deaths) |> arrange(Country, age_group)
all_07_f <- all_07_wide |> filter(Sex=="female") 
all_07_m <- all_07_wide |> filter(Sex=="male") 
all_22_wide <- allprop_2022 |> subset(select = -c(avoidable, total_deaths.o, proportion.o, total_deaths.c, proportion.c,
                                                  proportion.b, death_type, deaths_pyramid, prop.t, prop.t_pyramid)) |> 
  pivot_wider(names_from = fill_group, values_from = deaths) |> arrange(Country, age_group)
all_22_f <- all_22_wide |> filter(Sex=="female")
all_22_m <- all_22_wide |> filter(Sex=="male")

export(all_07_f, file = "data/all_fdeaths07.csv")
export(all_22_f, file = "data/all_fdeaths22.csv")

export(all_07_m, file = "data/all_mdeaths07.csv")
export(all_22_m, file = "data/all_mdeaths22.csv")

##if i want to add labels to the bars with percentage of ca deaths within each age group?
##do i just want avoidable ca deaths?
bar_totals07 <- allprop_2007 %>%
  filter(!age_group %in% c("75", "80", "85")) %>%
  group_by(Country, age_group, Sex) %>%
  summarise(
    total_prop = sum(prop.t_pyramid, na.rm = TRUE),
    .groups = "drop"
  )

cancer_labels07 <- allprop_2007 %>%
  filter(
    grepl("cancer", fill_group),          # keep cancer only
    fill_group != "not avoidable - cancer",
    !age_group %in% c("75", "80", "85")
  ) %>%
  group_by(Country, age_group, Sex) %>%
  summarise(
    cancer_prop = sum(prop.t_pyramid, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  left_join(bar_totals07, by = c("Country", "age_group", "Sex")) %>% 
  filter(abs(cancer_prop) > 0.001)



##plot07  <-  
allprop_2007 %>%
  filter(!age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = prop.t_pyramid, fill = fill_group)) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, colour = "white", linewidth = 0.5) + 
  scale_fill_manual(values = c(
    "preventable - cancer"   = "greenyellow",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "darkseagreen",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "#8A5E10",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
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
  scale_y_continuous(
    limits = c(-0.16, 0.17),
    breaks = seq(-0.15, 0.15, by = 0.05),
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  ) +
  geom_text(
    data = cancer_labels07,
    aes(
      x = age_group,
      y = ifelse(
        total_prop > 0,
        total_prop + 0.01,
        total_prop - 0.01
      ),
      label = scales::percent(abs(cancer_prop), accuracy = 0.1)
    ),
    inherit.aes = FALSE,
    hjust = ifelse(cancer_labels07$total_prop > 0, 0, 1),
    size = 3
  )+ coord_flip() +                       # flip so age groups are on y axis
  labs(
    x = "Age group",
    y = "Percentage of total deaths",
    fill = "Cause",
    caption = "← Female | Male →\nLabels show percentage of total deaths attributable to avoidable cancer within each age group",
    title = "Distribution of deaths attributable to avoidable causes for UK & Ireland - 2007",
  ) +
  theme_minimal() 

##without %
allprop_2007 %>%
  filter(!age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = prop.t_pyramid, fill = fill_group)) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, colour = "white", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "greenyellow",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "darkseagreen",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "#8A5E10",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
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
  scale_y_continuous(
    limits = c(-0.16, 0.17),
    breaks = seq(-0.15, 0.15, by = 0.05),
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  )+ coord_flip() +                       # flip so age groups are on y axis
  labs(
    x = "Age group",
    y = "Percentage of total deaths",
    fill = "Avoidability",
    caption = "← Female | Male →",
    title = "Distribution of deaths attributable to avoidable causes for UK & Ireland - 2007",
  ) +
  theme_minimal() 




##plot22 <- 
bar_totals22 <- allprop_2022 %>%
  filter(!age_group %in% c("75", "80", "85")) %>%
  group_by(Country, age_group, Sex) %>%
  summarise(
    total_prop = sum(prop.t_pyramid, na.rm = TRUE),
    .groups = "drop"
  )
cancer_labels22 <- allprop_2022 %>%
  filter(
    grepl("cancer", fill_group),          # keep cancer only
    fill_group != "not avoidable - cancer",
    !age_group %in% c("75", "80", "85")
  ) %>%
  group_by(Country, age_group, Sex) %>%
  summarise(
    cancer_prop = sum(prop.t_pyramid, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  left_join(bar_totals22, by = c("Country", "age_group", "Sex")) %>% 
  filter(abs(cancer_prop) > 0.001)

allprop_2022 %>%
  filter(!age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = prop.t_pyramid, fill = fill_group)) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, colour = "white", linewidth = 0.5) + 
  scale_fill_manual(values = c(
    "preventable - cancer"   = "greenyellow",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "darkseagreen",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "#8A5E10",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +
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
  scale_y_continuous(
    limits = c(-0.16, 0.17),
    breaks = seq(-0.15, 0.15, by = 0.05),
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  ) +
  geom_text(
    data = cancer_labels22,
    aes(
      x = age_group,
      y = ifelse(
        total_prop > 0,
        total_prop + 0.01,
        total_prop - 0.01
      ),
      label = scales::percent(abs(cancer_prop), accuracy = 0.1)
    ),
    inherit.aes = FALSE,
    hjust = ifelse(cancer_labels22$total_prop > 0, 0, 1),
    size = 3
  )+ coord_flip() +                       # flip so age groups are on y axis
  labs(
    x = "Age group",
    y = "Percentage of total deaths",
    fill = "Cause",
    caption = "← Female | Male →\nLabels show percentage of total deaths attributable to avoidable cancer within each age group",
    title = "Distribution of deaths attributable to avoidable causes for UK & Ireland - 2022",
  ) +
  theme_minimal() 


library(gridExtra)
grid.arrange(plot07, plot22)

##do we want axis ticks on all 4 facets? scales = "free" in facet _wrap to do this

##now need to create figure showing how total proportions change over time
##by country not by age group

##maybe try and have years side by side
allprop_2007 <- allprop_2007 %>% mutate(Year = "2007")
allprop_2022 <- allprop_2022 %>% mutate(Year = "2022")
allprop <- bind_rows(allprop_2007, allprop_2022)


##perhaps more clear to compare females/males separately
plot_f <- allprop %>% 
  filter(Sex=="female", !age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = Year, y = prop.t, fill = fill_group)) +
  facet_grid(~Country, labeller = labeller(Country = country_labs) ) +
  geom_bar(stat = "identity")  +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "greenyellow",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "darkseagreen",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +                       # flip so age groups are on y axis
  labs(
    x = "Year",
    y = "Proportion of total deaths",
    fill = "Avoidability",
    title = "Avoidable female deaths (between 0-75 years) as a proportion of total deaths in UK & Ireland"
  ) +
    scale_y_continuous(
    limits = c(0, 0.55),
    breaks = seq(0, 0.55, by = 0.05)
  ) +
  theme_bw()
plot_m <- allprop %>% 
  filter(Sex=="male", !age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = Year, y = prop.t, fill = fill_group)) +
  facet_grid(~Country, labeller = labeller(Country = country_labs)) +
  geom_bar(stat = "identity")  +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "greenyellow",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "darkseagreen",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +                       # flip so age groups are on y axis
  labs(
    x = "Year",
    y = "Proportion of total deaths",
    fill = "Avoidability",
    title = "Avoidable male deaths (between 0-75 years) as a proportion of total deaths in UK & Ireland"
  ) +
  
  scale_y_continuous(
    limits = c(0, 0.55),
    breaks = seq(0, 0.55, by = 0.05)
  ) +
  theme_bw()
grid.arrange(plot_f, plot_m)

##include deaths from all ages
plot_totalf <- allprop %>% 
  filter(Sex=="female") %>%
  ggplot(aes(x = Year, y = prop.t, fill = fill_group)) +
  facet_grid(~Country, labeller = labeller(Country = country_labs)) +
  geom_bar(stat = "identity")  +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "greenyellow",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "darkseagreen",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "#8A5E10",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +                       # flip so age groups are on y axis
  labs(
    x = "Year",
    y = "Proportion of total deaths",
    fill = "Avoidability",
    title = "Avoidable female deaths as a proportion of total deaths in UK & Ireland"
  ) +
  theme_bw()
plot_totalm <- allprop %>% 
  filter(Sex=="male") %>%
  ggplot(aes(x = Year, y = prop.t, fill = fill_group)) +
  facet_grid(~Country, labeller = labeller(Country = country_labs)) +
  geom_bar(stat = "identity")  +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "greenyellow",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "darkseagreen",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "#8A5E10",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +                       # flip so age groups are on y axis
  labs(
    x = "Year",
    y = "Proportion of total deaths",
    fill = "Avoidability",
    title = "Avoidable male deaths as a proportion of total deaths in UK & Ireland"
  ) +
  theme_bw()

grid.arrange(plot_totalf, plot_totalm)

##how to add number showing how cancer props change between years
##gives difference in 2022 for cancer deaths for males
##does include all not avoidable deaths
allprop_withca <- 
  allprop %>%
  filter(Sex == "male",
         grepl("cancer", fill_group)) %>%
  group_by(Country, Year) %>%
  summarise(cancer_prop = sum(prop.t), .groups = "drop") %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(change = cancer_prop - lag(cancer_prop)) %>% filter(!is.na(change))


allprop %>% 
  filter(Sex=="male") %>%
  ggplot(aes(x = Year, y = prop.t, fill = fill_group)) +
  facet_grid(~Country) +
  geom_bar(stat = "identity")  +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "greenyellow",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "forestgreen",
    "not avoidable - cancer" = "darkseagreen",
    "preventable - other"    = "goldenrod",
    "treatable - other"      = "darkgoldenrod",
    "50/50 - other"          = "lightgoldenrod",
    "not avoidable - other"  = "lightgrey"
  )) +                       # flip so age groups are on y axis
  labs(
    x = "Year",
    y = "Proportion of total deaths",
    fill = "Avoidability",
    title = "Avoidable male deaths (between 0-75 years) as a proportion of total deaths in UK & Ireland"
  ) +
  theme_bw() +
  geom_text(
    data = allprop_withca,
    aes(
      x = Year,
      y = 0.3,  # position at top of bars
      label = paste0(
        ifelse(change > 0, "+", ""),
        round(change * 100, 2), "%"
      )
    ),
    inherit.aes = FALSE,
    size = 3
  )

##############################################################################
##for covid sensitivity analysis - same for 2019
data2019 <- import(file = "data/avoidable_deaths_19.csv")
IR_2019 <- data2019 %>% filter(Country=="ireland") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))
EW_2019 <- data2019 %>% filter(Country=="england_wales") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))
SC_2019 <- data2019 %>% filter(Country=="scotland") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))
NI_2019 <- data2019 %>% filter(Country=="northern_irl") %>% 
  group_by(Sex) %>% mutate(prop.t = deaths/sum(deaths)) %>% ungroup() %>% 
  mutate(prop.t_pyramid = ifelse(Sex == "female", -prop.t, prop.t)) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  ))) %>% mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

allprop_2019 <- bind_rows(SC_2019, EW_2019, IR_2019, NI_2019)
export(allprop_2019, file = "data/avoidable_deaths_19.csv")






##############USE PART 7 CODE FOR MX RESULTS##########################
##make a plot for cancer specific mortality over time?
##need pop at risk for each country/sex by age group
library(HMDHFDplus)
myHMDusername <- "eleanor.lucas1@student.lshtm.ac.uk"
myHMDpassword <- "CYt$d3fR5_XQ8ih"

#read in pop data, make mid year averages, make 85+ open ended group
pop_IRL <- readHMDweb(
  CNTRY = "IRL",
  item = "Population5",
  username = myHMDusername,
  password = myHMDpassword
) %>% mutate(
  female = (Female1 + Female2)/2,
  male = (Male1 + Male2)/2,
  total = (Total1 + Total2)/2
) %>% subset(select = -c(Female1, Female2, Male1, Male2, Total1, Total2)) %>% 
  mutate(age_group = if_else(Age >= 85, "85", as.character(Age))) %>%
  group_by(Year, age_group) %>%
  summarise(
    female = sum(female, na.rm = TRUE),
    male   = sum(male,   na.rm = TRUE),
    total  = sum(total,  na.rm = TRUE),
    .groups = "drop") %>% 
  filter(Year %in% c("2007", "2022")) %>% 
  select(age_group, female, male, Year) %>%   # keep only what you need
  pivot_longer(
    cols      = c(female, male),
    names_to  = "sex",
    values_to = "population"
  ) %>%  
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  )))

pop_ENG_WA <- readHMDweb(
  CNTRY = "GBRTENW",
  item = "Population5",
  username = myHMDusername,
  password = myHMDpassword
) %>% mutate(
  female = (Female1 + Female2)/2,
  male = (Male1 + Male2)/2,
  total = (Total1 + Total2)/2
) %>% subset(select = -c(Female1, Female2, Male1, Male2, Total1, Total2)) %>% 
  mutate(age_group = if_else(Age >= 85, "85", as.character(Age))) %>%
  group_by(Year, age_group) %>%
  summarise(
    female = sum(female, na.rm = TRUE),
    male   = sum(male,   na.rm = TRUE),
    total  = sum(total,  na.rm = TRUE),
    .groups = "drop") %>% 
  filter(Year %in% c("2007", "2022")) %>% 
  select(age_group, female, male, Year) %>%   # keep only what you need
  pivot_longer(
    cols      = c(female, male),
    names_to  = "sex",
    values_to = "population"
  ) %>%  
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  )))
pop_SCO <- readHMDweb(
  CNTRY = "GBR_SCO",
  item = "Population5",
  username = myHMDusername,
  password = myHMDpassword
) %>% mutate(
  female = (Female1 + Female2)/2,
  male = (Male1 + Male2)/2,
  total = (Total1 + Total2)/2
) %>% subset(select = -c(Female1, Female2, Male1, Male2, Total1, Total2)) %>% 
  mutate(age_group = if_else(Age >= 85, "85", as.character(Age))) %>%
  group_by(Year, age_group) %>%
  summarise(
    female = sum(female, na.rm = TRUE),
    male   = sum(male,   na.rm = TRUE),
    total  = sum(total,  na.rm = TRUE),
    .groups = "drop") %>% 
  filter(Year %in% c("2007", "2022")) %>% 
  select(age_group, female, male, Year) %>%   # keep only what you need
  pivot_longer(
    cols      = c(female, male),
    names_to  = "sex",
    values_to = "population"
  )  %>% 
mutate(age_group = factor(age_group, levels = c(
  "0", "1", "5", "10", "15", "20", "25",
  "30", "35", "40", "45", "50", "55",
  "60", "65", "70", "75", "80", "85"
)))
pop_NIR <- readHMDweb(
  CNTRY = "GBR_NIR",
  item = "Population5",
  username = myHMDusername,
  password = myHMDpassword
) %>% mutate(
  female = (Female1 + Female2)/2,
  male = (Male1 + Male2)/2,
  total = (Total1 + Total2)/2
) %>% subset(select = -c(Female1, Female2, Male1, Male2, Total1, Total2)) %>% 
  mutate(age_group = if_else(Age >= 85, "85", as.character(Age))) %>%
  group_by(Year, age_group) %>%
  summarise(
    female = sum(female, na.rm = TRUE),
    male   = sum(male,   na.rm = TRUE),
    total  = sum(total,  na.rm = TRUE),
    .groups = "drop") %>% 
  filter(Year %in% c("2007", "2022")) %>% 
  select(age_group, female, male, Year) %>%   # keep only what you need
  pivot_longer(
    cols      = c(female, male),
    names_to  = "sex",
    values_to = "population"
  ) %>% 
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  )))

##making separate graphs for each country?
EW_2007 <- EW_2007 %>% mutate(Year = "2007")
EW_2022 <- EW_2022 %>% mutate(Year = "2022")
EW_dc <- bind_rows(EW_2022, EW_2007) %>% mutate(Year = as.numeric(Year))
EW_pop <- pop_ENG_WA %>% left_join(EW_dc, join_by(sex==Sex, Year==Year, age_group==age_group), multiple - "all", relationship = "many-to-many")
##number of cancer deaths in each age group/population in that age group *100000
EW_pop <- EW_pop %>% mutate(ca.mx = ((total_deaths.c/population)*100000))
EW_pop <- EW_pop %>% mutate(other.mx = ((total_deaths.o/population)*100000))


EW_pop %>%
  distinct(age_group, sex, Year, ca.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(!age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             colour = sex, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line() +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
    title = "Age-specific cancer mortality rates, England & Wales",
    colour = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  scale_y_log10()

IR_2007 <- IR_2007 %>% mutate(Year = "2007")
IR_2022 <- IR_2022 %>% mutate(Year = "2022")
IR_dc <- bind_rows(IR_2022, IR_2007) %>% mutate(Year = as.numeric(Year))
IR_pop <- pop_IRL %>% left_join(IR_dc, join_by(sex==Sex, Year==Year, age_group==age_group), multiple - "all", relationship = "many-to-many")
##number of cancer deaths in each age group/population in that age group *100000
IR_pop <- IR_pop %>% mutate(ca.mx = ((total_deaths.c/population)*100000))
IR_pop <- IR_pop %>% mutate(other.mx = ((total_deaths.o/population)*100000))


IR_pop %>%
  distinct(age_group, sex, Year, ca.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(!age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             colour = sex, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line() +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
    title = "Age-specific cancer mortality rates, Ireland",
    colour = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  scale_y_log10()
##log transofrm produced infinite values - age group with no cancer deaths


NIR_2007 <- NIR_2007 %>% mutate(Year = "2007")
NIR_2022 <- NIR_2022 %>% mutate(Year = "2022")
NIR_dc <- bind_rows(NIR_2022, NIR_2007) %>% mutate(Year = as.numeric(Year))
NIR_pop <- pop_NIR %>% left_join(NIR_dc, join_by(sex==Sex, Year==Year, age_group==age_group), multiple - "all", relationship = "many-to-many")
##number of cancer deaths in each age group/population in that age group *100000
NIR_pop <- NIR_pop %>% mutate(ca.mx = ((total_deaths.c/population)*100000))
NIR_pop <- NIR_pop %>% mutate(other.mx = ((total_deaths.o/population)*100000))


NIR_pop %>%
  distinct(age_group, sex, Year, ca.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(!age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             colour = sex, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line() +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
    title = "Age-specific cancer mortality rates, Northern Ireland",
    colour = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  scale_y_log10()


SCO_2007 <- SCO_2007 %>% mutate(Year = "2007")
SCO_2022 <- SCO_2022 %>% mutate(Year = "2022")
SCO_dc <- bind_rows(SCO_2022, SCO_2007) %>% mutate(Year = as.numeric(Year))
SCO_pop <- pop_SCO %>% left_join(SCO_dc, join_by(sex==Sex, Year==Year, age_group==age_group), multiple - "all", relationship = "many-to-many")
##number of cancer deaths in each age group/population in that age group *100000
SCO_pop <- SCO_pop %>% mutate(ca.mx = ((total_deaths.c/population)*100000))
SCO_pop <- SCO_pop %>% mutate(other.mx = ((total_deaths.o/population)*100000))

SCO_pop %>%
  distinct(age_group, sex, Year, ca.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(!age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             colour = sex, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line() +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
    title = "Age-specific cancer mortality rates, Scotland",
    colour = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  scale_y_log10()

##most useful from ages 30 upwards, too unstable under these ages?

ca.mx_combi <- bind_rows(EW_pop, IR_pop, NIR_pop, SCO_pop)
ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, ca.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(!age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             colour = sex, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line() +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
    title = "Age-specific cancer mortality rates, UK & Ireland",
    colour = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(30, 75)) +
  scale_y_log10()

##maybe separate into female and male
ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, ca.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(sex=="female", !age_group %in% c("0", "1", "5", "10", "15", "20", "25", "75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "violetred", linewidth = 0.6) +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
    title = "Age-specific cancer mortality rates for females 30 to 75 years: 2007 to 2022",
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



ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, ca.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(sex=="female", !age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "violetred", linewidth = 0.6) +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
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

ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, ca.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(sex=="male", !age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "skyblue3", linewidth = 0.6) +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
    title = "Age-specific cancer mortality rates for males up to 75 years:2007 to 2022",
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

##with smooth line
smooth_f <- ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, ca.mx) %>%
  filter(sex == "female", !age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(alpha = 0.4) +          # raw lines faded in background
  geom_smooth(se = FALSE, colour = "violetred", method = "loess", span = 0.75) +         # smooth line on top, no confidence band
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
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

smooth_m <- ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, ca.mx) %>%
  filter(sex == "male", !age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = ca.mx, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(alpha = 0.4) +          # raw lines faded in background
  geom_smooth(se = FALSE, colour = "skyblue3", method = "loess", span = 0.75) +         # smooth line on top, no confidence band
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate (per 100,000), log scale",
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

grid.arrange(smooth_f, smooth_m, ncol=2)

##may be useful to visualise other avoidable mx
##need to reorder age group again
ca.mx_combi <- ca.mx_combi %>% mutate(age_group = factor(age_group, levels = c(
  "0", "1", "5", "10", "15", "20", "25",
  "30", "35", "40", "45", "50", "55",
  "60", "65", "70", "75", "80", "85"
)))
ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, other.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(sex=="female", !age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = other.mx, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "violetred", linewidth = 0.6) +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "non-cancer mortality rate (per 100,000), log scale",
    title = "Age-specific non-cancer mortality rates for females up to 75 years: 2007 to 2022",
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

ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, other.mx) %>%   # keeps only unique combinations, one ca.mx per age sex year
  filter(sex=="male", !age_group %in% c("75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = other.mx, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "skyblue3", size = 0.6) +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Non-cancer mortality rate (per 100,000), log scale",
    title = "Age-specific non-cancer mortality rates for males up to 75 years: 2007 to 2022",
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

all.mx <- ca.mx_combi %>%
  distinct(Country, age_group, sex, Year, ca.mx, other.mx) 
ir.mx <- all.mx %>% filter(Country=="ireland")
ew.mx <- all.mx %>% filter(Country=="england_wales")
sc.mx <- all.mx %>% filter(Country=="scotland")
ni.mx <- all.mx %>% filter(Country=="northern_irl")

sc.mx <- sc.mx %>% mutate(RRca = (sc.mx$ca.mx/ir.mx$ca.mx)) %>% mutate(RRother = (sc.mx$other.mx/ir.mx$other.mx)) 
ni.mx <- ni.mx %>% mutate(RRca = (ni.mx$ca.mx/ir.mx$ca.mx)) %>% mutate(RRother = (ni.mx$other.mx/ir.mx$other.mx)) 
ew.mx <- ew.mx %>% mutate(RRca = (ew.mx$ca.mx/ir.mx$ca.mx)) %>% mutate(RRother = (ew.mx$other.mx/ir.mx$other.mx)) 
RR <- bind_rows(sc.mx, ni.mx, ew.mx)

RR %>%
  filter(sex=="male", !age_group %in% c("0", "1", "5", "10", "15", "20", "25","75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = RRca, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "skyblue3", size = 0.6) +
  facet_wrap(~Country) +
  labs(
      x = "Age group",
      y = "Cancer mortality rate ratio (compared to ROI)",
      title = "Cancer specific mortality rate ratios compared to ROI",
      linetype = "Year"
    ) +
  theme_bw()

RR %>%
  filter(sex=="female", !age_group %in% c("0", "1", "5", "10", "15", "20", "25","75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = RRca, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "violetred", size = 0.6) +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate ratio (compared to ROI)",
    title = "Cancer specific mortality rate ratios compared to ROI",
    linetype = "Year"
  ) +
  theme_bw()

RR %>%
  filter(sex=="male", !age_group %in% c("0", "1", "5", "10", "15", "20", "25","75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = RRother, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "skyblue3", size = 0.6) +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate ratio (compared to ROI)",
    title = "mortality rate ratios (excluding cancer) compared to ROI",
    linetype = "Year"
  ) +
  theme_bw()

RR %>%
  filter(sex=="female", !age_group %in% c("0", "1", "5", "10", "15", "20", "25","75", "80", "85")) %>%
  ggplot(aes(x = age_group, y = RRother, 
             linetype = factor(Year),
             group = interaction(sex, Year))) +
  geom_line(colour = "violetred", size = 0.6) +
  facet_wrap(~Country) +
  labs(
    x = "Age group",
    y = "Cancer mortality rate ratio (compared to ROI)",
    title = "mortality rate ratios (excluding cancer) compared to ROI",
    linetype = "Year"
  ) +
  theme_bw()