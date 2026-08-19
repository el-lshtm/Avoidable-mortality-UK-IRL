rm(list = ls())

setwd("C:/Users/gleno/OneDrive/Documents/ELLIE UNI/PROJECT")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)
library(gridExtra)
NEWage_causemx <- import(file = "data/NEWage_causemx.csv")

prev_othermx <- NEWage_causemx |> filter(fill_group=="preventable - other")
notav_camx <- NEWage_causemx |> filter(fill_group=="not avoidable - cancer")
notav_othermx <- NEWage_causemx |> filter(fill_group=="not avoidable - other")
av_camx <- NEWage_causemx |> filter(fill_group %in% c("preventable - cancer", "treatable - cancer", "50/50 - cancer")) 
treat_camx <- NEWage_causemx |> filter(fill_group=="treatable - cancer")
prev_camx <- NEWage_causemx |> filter(fill_group=="preventable - cancer")
treat_othermx <- NEWage_causemx |> filter(fill_group=="treatable - other")
fifty_camx <- NEWage_causemx |> filter(fill_group=="50/50 - cancer")
fifty_othermx <- NEWage_causemx |> filter(fill_group=="50/50 - other")
country_labs <- c("england_wales" = "England & Wales", "ireland" = "Republic of Ireland", "northern_irl" = "Northern Ireland", "scotland" = "Scotland")

treat_camx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Treatable cancer mortality rate, log scale",
    title = "Treatable cancer ASMRs (smoothed) for males in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

treat_camx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Treatable cancer mortality rate, log scale",
    title = "Treatable cancer ASMRs (smoothed) for females in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

prev_camx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Preventable cancer mortality rate, log scale",
    title = "Preventable cancer ASMRs (smoothed) for males in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

prev_camx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Preventable cancer mortality rate, log scale",
    title = "Preventable cancer ASMRs (smoothed) for females in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

treat_othermx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Treatable non-cancer mortality rate, log scale",
    title = "Treatable non-cancer ASMRs (smoothed) for males in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

treat_othermx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Treatable non-cancer mortality rate, log scale",
    title = "Treatable non-cancer ASMRs (smoothed) for females in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

fifty_othermx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "50/50 non-cancer mortality rate, log scale",
    title = "50/50 non-cancer ASMRs (smoothed) for males in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

fifty_othermx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "50/50 non-cancer mortality rate, log scale",
    title = "50/50 non-cancer ASMRs (smoothed) for females in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

fifty_camx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "50/50 cancer mortality rate, log scale",
    title = "50/50 cancer ASMRs (smoothed) for females in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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
                       
                                    
prev_othermx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Preventable non-cancer mortality rate, log scale",
    title = "Preventable non-cancer ASMRs (smoothed) for males in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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

prev_othermx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Preventable non-cancer mortality rate, log scale",
    title = "Preventable non-cancer ASMRs (smoothed) for females in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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




notav_camx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Not avoidable cancer mortality rate, log scale",
    title = "Not avoidable cancer ASMRs (smoothed) for females in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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
    "70" = "70-74",
    "75" = "75-79",
    "80" = "80-84",
    "85" = "85+"
  )) +
  scale_y_log10()

notav_camx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Not avoidable cancer mortality rate, log scale",
    title = "Not avoidable cancer ASMRs (smoothed) for males in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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
    "70" = "70-74",
    "75" = "75-79",
    "80" = "80-84",
    "85" = "85+"
  )) +
  scale_y_log10()


notav_othermx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="male") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Not avoidable non-cancer mortality rate, log scale",
    title = "Not avoidable non-cancer ASMRs (smoothed) for males in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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
    "70" = "70-74",
    "75" = "75-79",
    "80" = "80-84",
    "85" = "85+"
  )) +
  scale_y_log10() +
  theme(plot.title = element_text(size = 12))


notav_othermx %>% 
  mutate(age_group = as.factor(age_group)) %>% 
  filter(Sex=="female") %>% 
  ggplot(aes(x = age_group, y = age_causemx, 
             colour = factor(Year),
             group = interaction(Sex, Year))) +
  geom_point(shape = 5)  + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~Country, labeller = labeller(Country = country_labs)) +
  labs(
    x = "Age group",
    y = "Not avoidable non-cancer mortality rate, log scale",
    title = "Not avoidable non-cancer ASMRs (smoothed) for females in the UK and Ireland,\n2007 to 2022",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 8), strip.text = element_text(size = 12, face = "bold")) +
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
    "70" = "70-74",
    "75" = "75-79",
    "80" = "80-84",
    "85" = "85+"
  )) +
  scale_y_log10() +
  theme(plot.title = element_text(size = 12))