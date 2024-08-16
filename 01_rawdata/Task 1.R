# Population Data Function

library(readxl)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(psych)
library(GPArotation)
library(readr)
library(stringr)

setwd("~/GitHub/UNICEF-P3-assessment-public/01_rawdata")

read_population <- function() {
  population <- read_excel("WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx", skip = 16, col_names = TRUE)
  
  usecols <- c("ISO3 Alpha-code", "Type", "Year", "Female Population, as of 1 July (thousands)")
  
  population_filtered <- population %>%
    select(all_of(usecols)) %>%
    drop_na() %>%
    filter(Type == "Country/Area", Year >= 2018, Year <= 2022) %>%
    rename(Country = `ISO3 Alpha-code`, Population = `Female Population, as of 1 July (thousands)`) %>%
    mutate(Year = as.integer(Year)) %>%
    select(Country, Population, Year)
  
  return(population_filtered)
}

read_care_data <- function() {
  care_data <- read_csv("fusion_GLOBAL_DATAFLOW_UNICEF_1.0_.MNCH_ANC4+MNCH_SAB.F+M+_T.csv")
  
  usecols <- c("Country","REF_AREA:Geographic area", "INDICATOR:Indicator", "TIME_PERIOD:Time period", "OBS_VALUE:Observation Value")
  
  care_data_filtered <- care_data %>%
    select(all_of(usecols)) %>%
    drop_na() %>%
    mutate(`TIME_PERIOD:Time period` = as.integer(`TIME_PERIOD:Time period`)) %>%
    mutate(`REF_AREA:Geographic area` = sub(":.*$", "", `REF_AREA:Geographic area`)) %>%
    group_by(`REF_AREA:Geographic area`, `INDICATOR:Indicator`) %>%
    filter(`TIME_PERIOD:Time period` == max(`TIME_PERIOD:Time period`)) %>%
    ungroup() %>%
    rename(Country = `REF_AREA:Geographic area`, Indicator = `INDICATOR:Indicator`, Weight = `OBS_VALUE:Observation Value`) %>%
    care_data$Country <- as.character(care_data$Country) %>%
    select(Country, Indicator, Weight)
    
  return(care_data_filtered)
}

read_on_track_off_track <- function() {
  on_track_off_track <- read_excel("On-track and off-track countries.xlsx")
  
  usecols <- c("ISO3Code", "Status.U5MR")
  
  on_track_filtered <- on_track_off_track %>%
    select(all_of(usecols)) %>%
    drop_na() %>%
    mutate(On_Track = Status.U5MR %in% c("Achieved", "On Track")) %>%
    rename(Country = ISO3Code) %>%
    select(Country, On_Track)
  
  return(on_track_filtered)
}
read_care_data <- function() {
  care_data <- read_csv("fusion_GLOBAL_DATAFLOW_UNICEF_1.0_.MNCH_ANC4+MNCH_SAB.F+M+_T.csv")
  
  usecols <- c("REF_AREA:Geographic area", "INDICATOR:Indicator", "TIME_PERIOD:Time period", "OBS_VALUE:Observation Value")
  
  care_data_filtered <- care_data %>%
    select(all_of(usecols)) %>%
    drop_na() %>%
    mutate(`TIME_PERIOD:Time period` = as.integer(`TIME_PERIOD:Time period`)) %>%
    mutate(`REF_AREA:Geographic area` = sub(":.*$", "", `REF_AREA:Geographic area`)) %>%
    group_by(`REF_AREA:Geographic area`, `INDICATOR:Indicator`) %>%
    filter(`TIME_PERIOD:Time period` == max(`TIME_PERIOD:Time period`)) %>%
    ungroup() %>%
    rename(Country = `REF_AREA:Geographic area`, Indicator = `INDICATOR:Indicator`, Weight = `OBS_VALUE:Observation Value`) %>%
    select(Country, Indicator, Weight)
  
  return(care_data_filtered)
}

read_on_track_off_track <- function() {
  on_track_off_track <- read_excel("On-track and off-track countries.xlsx")
  
  usecols <- c("ISO3Code", "Status.U5MR")
  
  on_track_filtered <- on_track_off_track %>%
    select(all_of(usecols)) %>%drop_na() %>%
    mutate(On_Track = Status.U5MR %in% c("Achieved", "On Track")) %>%
    rename(Country = ISO3Code) %>%
    select(Country, On_Track)
  
  return(on_track_filtered)
}

population_data <- read_population()
care_data <- read_care_data()
on_track_data <- read_on_track_off_track()

care_data$Country <- as.character(care_data$Country)
on_track_data$Country <- as.character(on_track_data$Country)

df <- population_data %>%
  inner_join(care_data, by = "Country") %>%
  inner_join(on_track_data, by = "Country")

df <- df %>%
  mutate(Numerator = Weight * Population) %>%
  group_by(Country, Indicator, `On Track`) %>%
  summarise(Numerator = sum(Numerator), Population = sum(Population), .groups = 'drop') %>%
  mutate(`Weighted Coverage` = Numerator / Population) %>%
  select(Country, Indicator, `On Track`, `Weighted Coverage`)

write.xlsx(df, "weighted avg output.xlsx")