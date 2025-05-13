## Building Enrollment VDOE tables for ACPS Data Audit 
#
# All data were downloaded from Virginia Department of Education Fall Membership Tables
# https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304 

# Load libraries ----
library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)

# Load Feeder Pattern data

fp <- read.csv("data/tempdata/feeder_pattern.csv") %>% 
  clean_names() %>% 
  rename(division = division_name)

school_count <- read.csv("data/tempdata/school_count.csv") %>% 
  clean_names() %>% 
  rename(division = division_name, count = total_count, year = school_year) %>% 
  mutate(count = str_remove(count, ","),
       count = as.integer(count)) %>%  
  select(-ft_count, -pt_count)

feeder_count <- merge(school_count, fp, all.x=TRUE, all.y=TRUE) %>% 
  group_by(year, feeder_pattern) %>% 
  summarize(total_count = sum(count))


# RACE/ETHNICITY ENROLLMENT by FEEDER PATTERN

# ..................................................

# Query Parameters for Enrollment & Race/Ethnicity Enrollment
#  - School Years: 2011 - 2024 
#  - Report Level: School - Albemarle County - Select all schools except "Albemarle County Community Public Charter," "Community Lab School," Murray School," and "Murray High"
#  - Race: Disaggregated (when appropriate, change to All for totals)
#  - Gender: All 
#  - Grade: All
#  - Disadvantaged: All
#  - Foster Care: All
#  - Homeless: All
#  - Migrant: All
#  - Military Connected: All
#  - English Learners: All
#  - Disabled: All


#.....................................RACE.....................................

#### enrollment by race for FEEDER PATTERNS

albemarle_race <- read.csv("data/tempdata/school_race.csv") %>% 
  clean_names() %>% 
  rename(count = total_count, division = division_name, year = school_year) %>% 
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  group_by(year, school_number) %>% 
  mutate(num_students = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-ft_count, -pt_count, -num_students)

feeder_race <- merge(albemarle_race, fp, all.x=TRUE, all.y=TRUE) %>% 
  group_by(year, feeder_pattern, race) %>% 
  summarize(fp_count = sum(count))

# Calculate percent by race by feeder pattern
feeder_race_percent <- merge(feeder_race, feeder_count, all.x=TRUE, all.y=TRUE) %>% 
  mutate(fp_percent = (fp_count/total_count)*100, fp_percent = round(fp_percent, 1)) 

## Change Race Names
feeder_race_percent <- feeder_race_percent %>% 
      mutate(race = ifelse(race=="Black, not of Hispanic origin","Black", race))
feeder_race_percent <- feeder_race_percent %>% 
  mutate(race = ifelse(race=="Non-Hispanic, two or more races","Multiracial", race))
feeder_race_percent <- feeder_race_percent %>% 
  mutate(race = ifelse(race=="White, not of Hispanic origin", "White", race))
feeder_race_percent <- feeder_race_percent %>% 
  mutate(race = ifelse(race=="American Indian or Alaska Native", "American Indian", race))
feeder_race_percent <- feeder_race_percent %>% 
  mutate(race = ifelse(race=="Native Hawaiian  or Pacific Islander", "Pacific Islander", race))

# Write CSV for Feeder Pattern percent enrollment

write.csv(feeder_race_percent, "data/feeder_race_percent.csv")

#-------------------------------------------------------------------------------



#.......................ECONOMICALLY DISADVANTAGED by FEEDER PATTERN..............................


## Query Parameters from VDOE build-a-table
#  - School Years: 2011 - 2025 
#  - Report Level: School - Albemarle County - Select all schools except "Albemarle County Community Public Charter," "Community Lab School," Murray School," and "Murray High" 
#  - Race: All
#  - Gender: All 
#  - Grade: All
#  - Disadvantaged: Yes , No
#  - Foster Care: All
#  - Homeless: All
#  - Migrant: All
#  - Military Connected: All
#  - English Learners: All
#  - Disabled: All


#--------------------------------------------------------------------------------

econdis <- read_csv("data/tempdata/school_disadv.csv") %>% 
  clean_names() 

econdis <- econdis %>% 
  rename(count = total_count, division = division_name, year = school_year) %>% 
  select(-ft_count, -pt_count)

econdis <- econdis %>%  
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  group_by(year, school_number) %>% 
  mutate(num_students = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = disadvantaged, values_from = c(count)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% 
  select(-num_students)

feeder_econdis <- merge(econdis, fp, all.x=TRUE, all.y=TRUE) 

feeder_econdis_count <- feeder_econdis %>% 
  group_by(year, feeder_pattern) %>% 
  summarize(total_disadv_count = sum(Y))

feeder_econdis_percent <- merge(feeder_econdis_count, feeder_count, all.x=TRUE, all.y=TRUE) %>% 
  mutate(fp_percent = (total_disadv_count/total_count)*100, fp_percent = round(fp_percent, 1))

# Write CSV for enrollment of Economically Disadvantaged by Feeder Pattern

write.csv(feeder_econdis_percent, "data/feeder_disadvantaged_percent.csv")


#-------------------------------------------------------------------------------



#............................ENGLISH LEARNERS by FEEDER PATTERN...................................


# Query Parameters from VDOE membership build-a-table

#  - School Years: 2011 - 2025
#  - Report Level: School - Albemarle County - Select all schools except "Albemarle County Community Public Charter," "Community Lab School," Murray School," and "Murray High"
#  - Race: All
#  - Gender: All 
#  - Grade: All
#  - Disadvantaged: All
#  - Foster Care: All
#  - Homeless: All
#  - Migrant: All
#  - Military Connected: All
#  - English Learners: Yes, No (include former ELs)
#  - Disabled: All


#-------------------------------------------------------------------------------


ell <- read_csv("data/tempdata/school_ell.csv") %>% 
  clean_names() 

ell <- ell %>% 
  rename(count = total_count, division = division_name, year = school_year, ell = english_learners_include_former_e_ls) %>% 
  select(-ft_count, -pt_count)

ell <- ell %>%
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>%
  group_by(year, school_number) %>% 
  mutate(num_students = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ell, values_from = c(count)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% 
  select(-num_students)

feeder_ell <- merge(ell, fp, all.x=TRUE, all.y=TRUE) 

feeder_ell_count <- feeder_ell %>% 
  group_by(year, feeder_pattern) %>% 
  summarize(total_ell_count = sum(Y))

feeder_ell_percent <- merge(feeder_ell_count, feeder_count, all.x=TRUE, all.y=TRUE) %>% 
  mutate(fp_percent = (total_ell_count/total_count)*100, fp_percent = round(fp_percent, 1))


# Write CSV for enrollment of English Learners by Feeder Pattern

write.csv(feeder_ell_percent, "data/feeder_ell_percent.csv")

#-------------------------------------------------------------------------------

#..................................SPED.........................................


# Query Parameters from VDOE membership build-a-table

#  - School Years: 2011 - 2025 
#  - Report Level: School - Albemarle County - Select all schools except "Albemarle County Community Public Charter," "Community Lab School," Murray School," and "Murray High"
#  - Race: All
#  - Gender: All 
#  - Grade: All
#  - Disadvantaged: All
#  - Foster Care: All
#  - Homeless: All
#  - Migrant: All
#  - Military Connected: All
#  - English Learners: All
#  - Disabled: Yes , No


#-------------------------------------------------------------------------------


sped <- read_csv("data/tempdata/school_sped.csv") %>% 
  clean_names() 

sped <- sped %>% 
  rename(count = total_count, division = division_name, year = school_year, sped = disabled) %>% 
  select(-ft_count, -pt_count)

sped <- sped %>%
  mutate(count = str_remove(count, ","),
         count = as.integer(count)) %>%
  group_by(year, school_number) %>% 
  mutate(num_students = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sped, values_from = c(count)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% 
  select(-num_students)

feeder_sped <- merge(sped, fp, all.x=TRUE, all.y=TRUE) 

feeder_sped_count <- feeder_sped %>% 
  group_by(year, feeder_pattern) %>% 
  summarize(total_sped_count = sum(Y))

feeder_sped_percent <- merge(feeder_sped_count, feeder_count, all.x=TRUE, all.y=TRUE) %>% 
  mutate(fp_percent = (total_sped_count/total_count)*100, fp_percent = round(fp_percent, 1))

# Write CSV for enrollment of SPED by Feeder Pattern

write.csv(feeder_sped_percent, "data/feeder_sped_percent.csv")




