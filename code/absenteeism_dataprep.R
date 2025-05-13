## Building Absenteeism VDOE tables for ACPS Data Audit 
#
# All data were downloaded from Virginia Department of Education Site
# csv files provided by year

# Load libraries ----
library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)

## Load Data

absenteeism_2015 <- read_csv("data/tempdata/chronic_absenteeism_2015.csv") %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

absenteeism_2016 <- read_csv("data/tempdata/chronic_absenteeism_2016.csv")  %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

absenteeism_2017 <- read_csv("data/tempdata/chronic_absenteeism_2017.csv")  %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

absenteeism_2018 <- read_csv("data/tempdata/chronic_absenteeism_2018.csv") %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

absenteeism_2019 <- read_csv("data/tempdata/chronic_absenteeism_2019.csv") %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

absenteeism_2020 <- read_csv("data/tempdata/chronic_absenteeism_2020.csv") %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

absenteeism_2021 <- read_csv("data/tempdata/chronic_absenteeism_2021.csv") %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

absenteeism_2022 <- read_csv("data/tempdata/chronic_absenteeism_2022.csv") %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>%
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

absenteeism_2023 <- read_csv("data/tempdata/chronic_absenteeism_2023.csv") %>% 
  clean_names() %>% 
  rename(count = number_of_chronically_absent_students, percent = percent_chronically_absent) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(div_num == "2", grade == "All Students") %>% 
  filter(subgroup == "All Students" | subgroup == "Asian" | subgroup == "Black" | 
           subgroup == " Hispanic" | subgroup == "White"| subgroup == "Multiple Races" | 
           subgroup == "Economically Disadvantaged" | subgroup == "English Learners")

# rbind to combine dataframes 
total_absenteeism <- rbind(absenteeism_2015, absenteeism_2016, absenteeism_2017, absenteeism_2018,
                           absenteeism_2019, absenteeism_2020, absenteeism_2021, absenteeism_2022, absenteeism_2023)

#Change Multiple Races to Multiracial

total_absenteeism <- total_absenteeism %>% 
  mutate(subgroup = ifelse(subgroup=="Multiple Races", "Multiracial", subgroup))

# Write csv 

write_csv(total_absenteeism, "data/total_absenteeism.csv")



