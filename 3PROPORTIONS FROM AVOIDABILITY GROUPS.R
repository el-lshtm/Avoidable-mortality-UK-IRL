#looking at grouped cause of death proportions
rm(list = ls())

setwd("C:/Users/gleno/OneDrive/Documents/ELLIE UNI/PROJECT")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)

cancer2007 <- import(file = "data/cancercauses2007.csv")
other2007 <- import(file = "data/othercauses2007.csv")
cancer2022 <- import(file = "data/cancercauses2022.csv")
other2022 <- import(file = "data/othercauses2022.csv")

#calculating proportion of deaths in each age group that are preventable/treatable/5050/not avoidable
cancerprop2007 <- cancer2007 %>%
  filter(age_group != "all ages") %>%
  group_by(Country, Sex, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()
#reordering age groups
cancerprop2007 <- cancerprop2007 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85"
  )))

cancerprop2022 <- cancer2022 %>%
  filter(age_group != "all ages") %>%
  group_by(Country, Sex, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()

cancerprop2022 <- cancerprop2022 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85"
  )))

otherprop2007 <- other2007 %>%
  filter(age_group != "all ages") %>%
  group_by(Country, Sex, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()

otherprop2007 <- otherprop2007 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85"
  )))

otherprop2022 <- other2022 %>%
  filter(age_group != "all ages") %>%
  group_by(Country, Sex, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()

otherprop2022 <- otherprop2022 %>%
  mutate(age_group = factor(age_group, levels = c(
    "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", ">85"
  )))


#starting to epxlore proportions by separate country and sex - as stacked bar charts
cancerprop2007 %>% 
  filter(Country == "england_wales", Sex == "male") %>%
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
scale_fill_manual(values = c(
  "preventable"   = "seagreen",
  "treatable"     = "palegreen",
  "50/50"         = "green4",
  "not avoidable" = "grey"
)) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidable",
    title = "England/Wales; Proportion of cancer deaths by amenability and age group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#as expected no significant cancer deaths from 50/50 group
#more treatable cas at younger ages then more preventable ca deaths at middle ages

#comparing with non cancer deaths
otherprop2007 %>% 
  filter(Country == "england_wales", Sex == "male") %>%
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable"   = "deepskyblue1",
    "treatable"     = "steelblue4",
    "50/50"         = "dodgerblue1",
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidable",
    title = "Proportion of non-cancer deaths by avoidability and age group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#more from 50/50 group in middle ages - from cvd and similar

#trying to compare all countries/sexes in 1
otherprop2007 %>% 
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  facet_wrap(~Country + Sex) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable"   = "deepskyblue1",
    "treatable"     = "steelblue4",
    "50/50"         = "dodgerblue1",
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidable",
    title = "Proportion of non-cancer deaths by avoidability and age group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#comparing differences between timeframes

#have to bind them first?
cancer_combined <- bind_rows(
  cancerprop2007 %>% mutate(year = 2007),
  cancerprop2022 %>% mutate(year = 2022)
)

cancer_combined %>%
  filter(Country == "england_wales", Sex == "male") %>%
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 1) +  # stacked vertically for easy comparison
  scale_fill_manual(values = c(
    "preventable"   = "seagreen",
    "treatable"     = "palegreen",
    "50/50"         = "green4", 
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidability",
    title = "Cancer deaths by avoidability — England & Wales, Male"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#no huge differences in proportion, perhaps in total numbers there would be?

cancer_combined %>%
  filter(Country == "england_wales", Sex == "male") %>%
  ggplot(aes(x = age_group, y = total_deaths, fill = avoidable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 1) +  # stacked vertically for easy comparison
  scale_fill_manual(values = c(
    "preventable"   = "seagreen",
    "treatable"     = "palegreen",
    "50/50"         = "green4", 
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Total deaths",
    fill = "Avoidability",
    title = "Cancer deaths by avoidability — England & Wales, Male"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#and for women?
cancer_combined %>%
  filter(Country == "england_wales", Sex == "female") %>%
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 1) +  # stacked vertically for easy comparison
  scale_fill_manual(values = c(
    "preventable"   = "seagreen",
    "treatable"     = "palegreen",
    "50/50"         = "green4", 
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidability",
    title = "Cancer deaths by avoidability — England & Wales, Female"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#reduction in 50/50 deaths - cervical ca?


#perhaps more obvious differences between 2007 and 2022 in non cancer deaths

other_combined <- bind_rows(
  otherprop2007 %>% mutate(year = 2007),
  otherprop2022 %>% mutate(year = 2022)
)

other_combined %>%
  filter(Country == "england_wales", Sex == "male") %>%
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 1) +  # stacked vertically for easy comparison
  scale_fill_manual(values = c(
    "preventable"   = "deepskyblue1",
    "treatable"     = "steelblue4",
    "50/50"         = "dodgerblue1",
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidability",
    title = "Non-cancer deaths by avoidability — England & Wales, Male"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#see ifireland fares better
other_combined %>%
  filter(Country == "ireland") %>%
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year + Sex, ncol = 2) +  # stacked vertically for easy comparison
  scale_fill_manual(values = c(
    "preventable"   = "deepskyblue1",
    "treatable"     = "steelblue4",
    "50/50"         = "dodgerblue1",
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidability",
    title = "Non-cancer deaths by avoidability — Ireland"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#looks like visually there are more obvious improvements in avoidable deaths for men and women in ireland - how about for cas?
cancer_combined %>%
      filter(Country == "ireland") %>%
      ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ year + Sex, ncol = 2) +  # stacked vertically for easy comparison
      scale_fill_manual(values = c(
        "preventable"   = "seagreen",
        "treatable"     = "palegreen",
        "50/50"         = "green4", 
        "not avoidable" = "grey"
      )) +
      labs(
        x = "Age group",
        y = "Proportion of deaths",
        fill = "Avoidability",
        title = "Cancer deaths by avoidability — Ireland"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

#try and put all of this in one
other_combined %>% 
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
  facet_grid(~Country + Sex ~ year) +  # grid more organised
  scale_fill_manual(values = c(
    "preventable"   = "deepskyblue1",
    "treatable"     = "steelblue4",
    "50/50"         = "dodgerblue1",
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidability",
    title = "Non-cancer deaths by avoidability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

other_combined %>% 
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Country + Sex + year, ncol = 4) +  # maybe wrap works better?
  scale_fill_manual(values = c(
    "preventable"   = "deepskyblue1",
    "treatable"     = "steelblue4",
    "50/50"         = "dodgerblue1",
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidability",
    title = "Non-cancer deaths by avoidability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cancer_combined %>% 
  ggplot(aes(x = age_group, y = proportion, fill = avoidable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Country + Sex + year, ncol = 4) +  
  scale_fill_manual(values = c(
    "preventable"   = "seagreen",
    "treatable"     = "palegreen",
    "50/50"         = "green4", 
    "not avoidable" = "grey"
  )) +
  labs(
    x = "Age group",
    y = "Proportion of deaths",
    fill = "Avoidability",
    title = "cancer deaths by avoidability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##thoughts after initial viisualisations
##ireland performed better for non cancer deaths between 2007 and 2022, improvements across countries re cancer deaths seem more similar
##some non cancer avoidable deaths look to have increased - adult women in scotland and england/wales
##problems with not enough data re cancer deaths - particularly in NI 


#should make total of all groups as a proportion of each age group
#why do other_combined and cancer_combined not have the same no of observations

counts <- full_join(
  other_combined %>% count(avoidable, name = "n_df1"),
  cancer_combined %>% count(avoidable, name = "n_df2"),
  by = "avoidable"
) %>%
  mutate(diff = n_df1 - n_df2)

# Only show mismatches
counts %>% filter(diff != 0 | is.na(diff))

##for some reason cancer df is missing some 50/50 variables - no 50/50 group for males in cancer 
##makes sense as only 50/50 cancer is cervical - n/a to males - 

##try left join other to cancer

combi_07 <- left_join(otherprop2007, cancerprop2007, 
                      join_by(Country==Country, Sex == Sex, age_group==age_group, avoidable==avoidable),
                      suffix = c(".o", ".c"))

##all 50/50 cancers for males have NA values - sub these to 0 then update proportions/totals
combi_07 <- combi_07 %>% mutate(deaths.c = ifelse(is.na(deaths.c), 0, deaths.c))
combi_07 <- combi_07 %>% mutate(total_deaths.c = ifelse(is.na(total_deaths.c), 0, total_deaths.c))
combi_07 <- combi_07 %>% mutate(proportion.c = ifelse(is.na(proportion.c), 0, proportion.c))

combi_07 <- combi_07 %>% group_by(Country, Sex, age_group) %>% mutate(
  total_deaths.c = sum(deaths.c),
  proportion.c = (deaths.c / total_deaths.c)
)

combi_07 <- combi_07 %>% group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths.b = (total_deaths.o + total_deaths.c),
    proportion.b = (deaths.o + deaths.c) / total_deaths.b
  ) %>%
  ungroup()


##doing the same for 2022
combi_22 <- left_join(otherprop2022, cancerprop2022, 
                      join_by(Country==Country, Sex == Sex, age_group==age_group, avoidable==avoidable),
                      suffix = c(".o", ".c"))

combi_22 <- combi_22 %>% replace(is.na(.), 0)
combi_22 <- combi_22 %>% group_by(Country, Sex, age_group) %>% mutate(
  total_deaths.c = sum(deaths.c),
  proportion.c = (deaths.c / total_deaths.c)
)


combi_22 <- combi_22 %>% group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths.b = (total_deaths.o + total_deaths.c),
    proportion.b = (deaths.o + deaths.c) / total_deaths.b
  ) %>%
  ungroup()

combi_07 %>%
  ggplot(aes(x = age_group, y = deaths.o, fill = avoidable)) +
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

##can use this to get proportion of total deaths but only by other or cancer
##need to pivto longer so deaths are all one long column - then can plot both other and cancer deaths in same stack

combi_07_long <- combi_07 %>%
  pivot_longer(
    cols = c(deaths.c, deaths.o),
    names_to = "death_type",
    values_to = "deaths"
  ) %>%
  mutate(death_type = recode(death_type,
                             "deaths.c" = "cancer",
                             "deaths.o" = "other"
  ))

combi_07_long <- combi_07_long %>%
  mutate(fill_group = paste(avoidable, death_type, sep = " - "))



combi_07_long %>%
  ggplot(aes(x = age_group, y = deaths, fill = fill_group)) +
  facet_wrap(~ Country) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "seagreen",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "green4",
    "not avoidable - cancer" = "grey",
    "preventable - other"    = "steelblue",
    "treatable - other"      = "lightblue",
    "50/50 - other"          = "dodgerblue",
    "not avoidable - other"  = "lightgrey"
  )) +
  labs(
    x = "Age group",
    y = "Total deaths",
    fill = "Avoidability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##hard to visualise bc total non avoidable deaths over age of 75 so much larger

combi_07_long %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>% 
  ggplot(aes(x = age_group, y = deaths, fill = fill_group)) +
  facet_wrap(~ Country) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "preventable - cancer"   = "lawngreen",
    "treatable - cancer"     = "palegreen",
    "50/50 - cancer"         = "green4",
    "not avoidable - cancer" = "grey",
    "preventable - other"    = "steelblue",
    "treatable - other"      = "slateblue1",
    "50/50 - other"          = "dodgerblue",
    "not avoidable - other"  = "lightcyan1"
  )) +
  labs(
    x = "Age group",
    y = "Total deaths",
    fill = "Avoidability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#better but ordering is confusing
#trying to reorder avoidability so its groupied into cancer and non cancer

combi_07_long <- combi_07_long %>%
  mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

##colours still too messy
##better colours in more sensible order,facetted by sex
combi_07_long %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>% 
  ggplot(aes(x = age_group, y = deaths, fill = fill_group)) +
  facet_grid(Country ~ Sex) +
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
    title = "Cancer and non-cancer deaths within the UK and Ireland - 2007"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##do the same for 2022 - pivot longer etc


combi_22_long <- combi_22 %>%
  pivot_longer(
    cols = c(deaths.c, deaths.o),
    names_to = "death_type",
    values_to = "deaths"
  ) %>%
  mutate(death_type = recode(death_type,
                             "deaths.c" = "cancer",
                             "deaths.o" = "other"
  ))

combi_22_long <- combi_22_long %>%
  mutate(fill_group = paste(avoidable, death_type, sep = " - "))

combi_22_long <- combi_22_long %>%
  mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

combi_22_long %>%
  filter(!age_group %in% c("75-79", "80-84", ">85")) %>% 
  ggplot(aes(x = age_group, y = deaths, fill = fill_group)) +
  facet_grid(Country ~ Sex) +
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
    title = "Cancer and non-cancer deaths within the UK and Ireland - 2022"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##maybe would look better as a pop pyramid style
combi_22_sex <- combi_22_long %>%
  mutate(deaths_pyramid = ifelse(Sex == "female", -deaths, deaths))
##females nos now all negative

plot22 <- combi_22_sex %>%
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
    title = "2022 - Deaths within the UK and Ireland by avoidability"
  ) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.25)) 

##the same for 2007
combi_07_sex <- combi_07_long %>%
  mutate(deaths_pyramid = ifelse(Sex == "female", -deaths, deaths))


plot07 <- combi_07_sex %>%
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
    title = "2007 - Deaths within the UK and Ireland by avoidability, Cancer and Other causes"
  ) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.25)) 

#try to combine all years on same plot?
library(gridExtra)
grid.arrange(plot07, plot22, ncol = 2)
# messy with legend size etc

ggsave("2007fig.pdf", plot = plot07)
ggsave("2022fig.pdf", plot = plot22)

##maybe just try en&w both years on same plot as these are the biggest
EW07 <- combi_07_sex %>%
  filter(Country == "england_wales", !age_group %in% c("75-79", "80-84", ">85")) %>%
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
    title = "2007 - Deaths within England and Wales"
  ) +
  theme_classic() +
  theme(legend.position = "none")

EW22 <- combi_22_sex %>%
  filter(Country == "england_wales", !age_group %in% c("75-79", "80-84", ">85")) %>%
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
    title = "2022 - Deaths within England and Wales"
  ) +
  theme_classic() +
  theme(legend.position = "none")

grid.arrange(EW22, EW07, ncol = 2)

##trying to present deaths as a proportion of total deaths - need to make new column
##proportion of total deaths for all 8 possible cause groups 

combinedprop22 <- combi_22_sex %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    proportion.b = deaths_pyramid / total_deaths.b
  ) %>%
  ungroup()

prop22 <- combinedprop22 %>%
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
    title = "2022 - Proportion of deaths within the UK and Ireland by avoidability, Cancer and Other causes"
  ) +
  theme_classic()  

combinedprop07 <- combi_07_sex %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    proportion.b = deaths_pyramid / total_deaths.b
  ) %>%
  ungroup()

prop07 <- combinedprop07 %>%
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
    title = "2007 - Proportion of deaths within the UK and Ireland by avoidability, Cancer and Other causes"
  ) +
  theme_classic() 

grid.arrange(prop07, prop22, ncol = 2)

ggsave("2007figproportions.pdf", plot = prop07)
ggsave("2022figproportions.pdf", plot = prop22)


######for covid sensitivity analysis - doing the same tranformaitons to 2019
cancer2019 <- import(file = "data/cancercauses2019.csv")
other2019 <- import(file = "data/othercauses2019.csv")
cancer2019 <-  cancer2019 |> filter(age_group !="all ages",) 
other2019 <-  other2019 |> filter(age_group !="all ages",) 
#calculating proportion of deaths in each age group that are preventable/treatable/5050/not avoidable
cancerprop2019 <- cancer2019 %>%
  group_by(Country, Sex, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()
#reordering age groups
cancerprop2019 <- cancerprop2019  %>%
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  )))

otherprop2019 <- other2019 %>%
  group_by(Country, Sex, age_group, avoidable) %>%
  summarise(deaths = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths = sum(deaths, na.rm = TRUE),
    proportion = deaths / total_deaths
  ) %>%
  ungroup()


otherprop2019 <- otherprop2019  %>%
  mutate(age_group = factor(age_group, levels = c(
    "0", "1", "5", "10", "15", "20", "25",
    "30", "35", "40", "45", "50", "55",
    "60", "65", "70", "75", "80", "85"
  )))

##combining other and ca causes
combi2019 <- left_join(otherprop2019, cancerprop2019, 
                      join_by(Country==Country, Sex == Sex, age_group==age_group, avoidable==avoidable),
                      suffix = c(".o", ".c"))

##all 50/50 cancers for males have NA values - sub these to 0 then update proportions/totals
combi2019 <- combi2019 %>% mutate(deaths.c = ifelse(is.na(deaths.c), 0, deaths.c))
combi2019 <- combi2019 %>% mutate(total_deaths.c = ifelse(is.na(total_deaths.c), 0, total_deaths.c))
combi2019 <- combi2019 %>% mutate(proportion.c = ifelse(is.na(proportion.c), 0, proportion.c))

combi2019 <- combi2019 %>% group_by(Country, Sex, age_group) %>% mutate(
  total_deaths.c = sum(deaths.c),
  proportion.c = (deaths.c / total_deaths.c)
)

combi2019 <- combi2019 %>% group_by(Country, Sex, age_group) %>%
  mutate(
    total_deaths.b = (total_deaths.o + total_deaths.c),
    proportion.b = (deaths.o + deaths.c) / total_deaths.b
  ) %>%
  ungroup()

##pivot longer

combi2019_long <- combi2019 %>%
  pivot_longer(
    cols = c(deaths.c, deaths.o),
    names_to = "death_type",
    values_to = "deaths"
  ) %>%
  mutate(death_type = recode(death_type,
                             "deaths.c" = "cancer",
                             "deaths.o" = "other"
  ))

combi2019_long <- combi2019_long %>%
  mutate(fill_group = paste(avoidable, death_type, sep = " - "))


combi2019_long <- combi2019_long %>%
  mutate(fill_group = factor(fill_group, levels = c(
    "treatable - other", "preventable - other", "50/50 - other", 
    "not avoidable - other", "not avoidable - cancer", "treatable - cancer",
    "preventable - cancer", "50/50 - cancer" )))

combi2019_sex <- combi2019_long %>%
  mutate(deaths_pyramid = ifelse(Sex == "female", -deaths, deaths))

combinedprop19 <- combi2019_sex %>%
  group_by(Country, Sex, age_group) %>%
  mutate(
    proportion.b = deaths_pyramid / total_deaths.b
  ) %>%
  ungroup()

export(combi2019_sex, file = "data/avoidable_deaths_19.csv")


export(combi_07_sex, file = "data/avoidable_deaths_07.csv")
export(combi_22_sex, file = "data/avoidable_deaths_22.csv")
