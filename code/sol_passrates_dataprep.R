## Building SOL Pass Rate VDOE tables for ACPS Data Audit 
#
# All data were downloaded from Virginia Department of Education SOL Tables

#Query Parameters
#  - School Years: 2010 - 2024 
#  - Report Level: State and then Division - Albemarle County 
#  - Race: Disaggregated (when appropriate, change to All for totals)
#  - Gender: All 
#  - Grade: All
#  - Disadvantaged: (Yes, No when appropriate)
#  - Foster Care: All
#  - Homeless: All
#  - Migrant: All
#  - Military Connected: All
#  - English Learners: All (Yes, No when appropriate - include Former ELs)
#  - Disabled: (Yes, No when appropriate)

# Test Level: All Levels
# Test Source: SOL
# Subject Area: English: Reading OR Mathematics (when appropriate)
# Test: All Tests

# Statistic: Total Count, Pass Rate, Pass Count

# Load libraries ----
library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)


# READING SOL PASS RATES

# SOL Reading Pass Rates by ACPS and VA 2010- 2024

va_reading_pass <- read_csv("data/tempdata/va_reading_sol.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  mutate(division_number = 0, division_name = "Virginia") %>% 
  select(-test_source)

reading_pass <- read_csv("data/tempdata/sol_reading.csv") %>% 
  clean_names() %>% 
  rename(year = school_year)

reading_pass <- rbind(va_reading_pass, reading_pass)

write.csv(reading_pass, "data/reading_sol_pass.csv")

# SOL Reading Pass Rates by Race/Ethnicity

race_reading_pass <- read_csv("data/tempdata/sol_reading_race.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  filter(race == "Asian" | race == "Black, not of Hispanic origin" | 
           race == "Hispanic" | race == "White, not of Hispanic origin"| race == "Non-Hispanic, two or more races") %>% 
  select(-test_source)

## Change Race Names
race_reading_pass <- race_reading_pass %>% 
  mutate(race = ifelse(race=="Black, not of Hispanic origin","Black", race))
race_reading_pass <- race_reading_pass %>% 
  mutate(race = ifelse(race=="Non-Hispanic, two or more races","Multiracial", race))
race_reading_pass <- race_reading_pass %>% 
  mutate(race = ifelse(race=="White, not of Hispanic origin", "White", race))
race_reading_pass <- race_reading_pass %>% 
  mutate(race = ifelse(race=="American Indian or Alaska Native", "American Indian", race))
race_reading_pass <- race_reading_pass %>% 
  mutate(race = ifelse(race=="Native Hawaiian  or Pacific Islander", "Pacific Islander", race))

write.csv(race_reading_pass, "data/reading_sol_pass_race.csv")

# SOL Reading Pass Rates by English Learner

ell_reading_pass <- read_csv("data/tempdata/sol_reading_ell.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  add_column(data_level= "English Learner") %>% 
  mutate(english_learners=ifelse(english_learners=="Y","English Learner","Non-English Learner")) %>%
  rename(subgroup=english_learners) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  select(-test_source)

# SOL Reading Pass Rates by Economically Disadvantaged

disadv_reading_pass <- read_csv("data/tempdata/sol_reading_disadvantaged.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  add_column(data_level= "Economic Advantage") %>% 
  mutate(disadvantaged=ifelse(disadvantaged=="Y","Economically Disadvantaged","Non-ECD")) %>%
  rename(subgroup=disadvantaged) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  select(-test_source)

# SOL Reading Pass Rates by Disabled

sped_reading_pass <- read_csv("data/tempdata/sol_reading_sped.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  add_column(data_level= "SPED") %>% 
  mutate(disabled=ifelse(disabled=="Y","SPED","Non-SPED")) %>%
  rename(subgroup=disabled) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  select(-test_source)

# SOL Reading Pass Rates by ACPS
acps_reading_subgroup <- read_csv("data/tempdata/sol_reading.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  add_column(data_level= "ACPS") %>%
  add_column(subgroup= "ACPS")

# Combine subgroups
reading_subgroups <- merge(ell_reading_pass, disadv_reading_pass, all.x=TRUE, all.y=TRUE)
reading_subgroups <- merge(reading_subgroups, sped_reading_pass, all.x=TRUE, all.y=TRUE)
reading_subgroups <- merge(reading_subgroups, acps_reading_subgroup, all.x=TRUE, all.y=TRUE)

write.csv(reading_subgroups, "data/reading_sol_pass_subgroups.csv")

#________________________________________________________________________________

# Math SOL PASS RATES

# SOL Math Pass Rates by ACPS and VA

va_math_pass <- read_csv("data/tempdata/va_sol_math.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  mutate(division_number = 0, division_name = "Virginia") %>% 
  select(-test_source)

math_pass <- read_csv("data/tempdata/sol_math.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  select(-test_source)

math_pass <- rbind(va_math_pass, math_pass)

write.csv(reading_pass, "data/math_sol_pass.csv")

# SOL Math Pass Rates by Race/Ethnicity

race_math_pass <- read_csv("data/tempdata/sol_math_race.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  filter(race == "Asian" | race == "Black, not of Hispanic origin" | 
        race == "Hispanic" | race == "White, not of Hispanic origin"| race == "Non-Hispanic, two or more races") %>% 
  select(-test_source)

## Change Race Names
race_math_pass <- race_math_pass %>% 
  mutate(race = ifelse(race=="Black, not of Hispanic origin","Black", race))
race_math_pass <- race_math_pass %>% 
  mutate(race = ifelse(race=="Non-Hispanic, two or more races","Multiracial", race))
race_math_pass <- race_math_pass %>% 
  mutate(race = ifelse(race=="White, not of Hispanic origin", "White", race))
race_math_pass <- race_math_pass %>% 
  mutate(race = ifelse(race=="American Indian or Alaska Native", "American Indian", race))
race_math_pass <- race_math_passs %>% 
  mutate(race = ifelse(race=="Native Hawaiian  or Pacific Islander", "Pacific Islander", race))

write.csv(race_math_pass, "data/math_sol_pass_race.csv")

# SOL Math Pass Rates by English Learner

ell_math_pass <- read_csv("data/tempdata/sol_math_ell.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  add_column(data_level= "English Learner") %>% 
  mutate(english_learners=ifelse(english_learners=="Y","English Learner","Non-English Learner")) %>%
  rename(subgroup=english_learners) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  select(-test_source)

# SOL Math Pass Rates by Economically Disadvantaged

disadv_math_pass <- read_csv("data/tempdata/sol_math_disadvantaged.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  add_column(data_level= "Economic Advantage") %>% 
  mutate(disadvantaged=ifelse(disadvantaged=="Y","Economically Disadvantaged","Non-ECD")) %>%
  rename(subgroup=disadvantaged) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  select(-test_source)

# SOL Math Pass Rates by SPED

sped_math_pass <- read_csv("data/tempdata/sol_math_sped.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  add_column(data_level= "SPED") %>% 
  mutate(disabled=ifelse(disabled=="Y","SPED","Non-SPED")) %>%
  rename(subgroup=disabled) %>% 
  mutate(pass_rate = as.integer(pass_rate)) %>% 
  select(-c(division_number, division_name)) %>% 
  select(-test_source)

# SOL Math Pass Rates by ACPS
acps_math_subgroup <- read_csv("data/tempdata/va_sol_math.csv") %>% 
  clean_names() %>% 
  rename(year = school_year) %>% 
  add_column(data_level= "Virginia") %>%
  add_column(subgroup= "Virginia") %>% 
  select(-test_source)

#Combine Subgroups

math_subgroups <- merge(ell_math_pass, disadv_math_pass, all.x=TRUE,all.y=TRUE)
math_subgroups <- merge(math_subgroups, sped_math_pass, all.x=TRUE,all.y=TRUE)
math_subgroups <- merge(math_subgroups, acps_math_subgroup, all.x=TRUE,all.y=TRUE)

write.csv(math_subgroups, "data/math_sol_pass_subgroups.csv")



