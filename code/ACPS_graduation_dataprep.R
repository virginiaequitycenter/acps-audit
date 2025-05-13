##############################################################
# ACPS EQUITY Audit - Graduation and Dropout Rates        
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-03-18
# Summary: A document that creates a group of datasets from the
#          Cohort Graduation Build-a-Table for ACPS Equity Audit
##############################################################

## data from VDOE Cohort Graduation Build-a-Table:
## https://p1pe.doe.virginia.gov/apex/f?p=246:1:108853500518675::::: 

### Query parameters:

# Cohort Year(s) = 2008 through 2024
# Report Level: Division, School, and State
# Select Student Characteristics = create a table with all racial categories
# Reporting Categories = create separate tables for Disadvantaged, English Learner, Disabled 
# Type of Graduation Rate = Virginia On-Time Graduation Rate, 4 years
# Statistic: Graduation Rate, Dropout Rate, Diplomas Count, Total Graduates, Students in Cohort

# Output: Graduation Rate CSVs for all groups, and then subgroups: by race, 
#         economic advantage, disability status, and english learner status

##############################################################
# Call Libraries
##############################################################

library(readxl)
library(dplyr)
library(tidyverse)
library(janitor)

##############################################################
# Call and Clean Datasets
##############################################################

grad_all <- read_csv("raw_data/ACPS_Graduation_Rates_all_0824.csv") %>% 
            clean_names() %>% 
            add_column(data_level="All") %>%
            add_column(subgroup="None")

grad_race <- read_csv("raw_data/ACPS_Graduation_Rates_race_0824.csv") %>% 
             clean_names() %>%
             add_column(data_level="Race") %>%
             rename(subgroup=race)
          
grad_disadv <- read_csv("raw_data/ACPS_Graduation_Rates_disadv_0824.csv") %>%
               clean_names() %>%
               add_column(data_level="Economic Advantage") %>%
               mutate(disadvantaged=ifelse(disadvantaged=="Y","Economically Disadvantaged","Non-ECD")) %>%
               rename(subgroup=disadvantaged)

grad_disabled <- read_csv("raw_data/ACPS_Graduation_Rates_disabled_0824.csv") %>%
                 clean_names() %>%
                 add_column(data_level="Disability") %>%
                 mutate(disabled=ifelse(disabled=="Y","Disabled","Non-Disabled")) %>%
                 rename(subgroup=disabled)

grad_el <- read_csv("raw_data/ACPS_Graduation_Rates_el_0824.csv")%>% 
           clean_names() %>%
           add_column(data_level="English Learner") %>%
           mutate(english_learner=ifelse(english_learner=="Y","English Learner","Non-English Learner")) %>%
           rename(subgroup=english_learner)

##############################################################
# Merge Datasets
##############################################################

grad_all_subgroups <- merge(grad_all,grad_race, all.x=TRUE,all.y=TRUE)
grad_all_subgroups <- merge(grad_all_subgroups,grad_disadv, all.x=TRUE,all.y=TRUE)
grad_all_subgroups <- merge(grad_all_subgroups,grad_disabled, all.x=TRUE,all.y=TRUE)
grad_all_subgroups <- merge(grad_all_subgroups,grad_el, all.x=TRUE,all.y=TRUE)

##############################################################
# Change Variable Types
##############################################################


grad_all_subgroups$graduation_rate <- sapply(grad_all_subgroups$graduation_rate, function(x) as.numeric(gsub("%", "", x)))
grad_all_subgroups$dropout_rate <- sapply(grad_all_subgroups$dropout_rate, function(x) as.numeric(gsub("%", "", x)))

grad_all_subgroups <- grad_all_subgroups %>% 
                      mutate_at(vars("students_in_cohort","total_graduates",
                                     "advanced_studies","ib","standard","other_diplomas",
                                     "applied_studies"),as.numeric)

grad_all_subgroups <- grad_all_subgroups %>% add_column(perc_advanced=
                                                        grad_all_subgroups$advanced_studies*100/
                                                        grad_all_subgroups$total_graduates)

#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2008","2007-08",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2009","2008-09",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2010","2009-10",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2011","2010-11",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2012","2011-12",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2013","2012-13",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2014","2013-14",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2015","2014-15",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2016","2015-16",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2017","2016-17",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2018","2017-18",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2019","2018-19",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2020","2019-20",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2021","2020-21",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2022","2021-22",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2023","2022-23",cohort_year))
#grad_all_subgroups <- grad_all_subgroups %>% mutate(cohort_year=ifelse(cohort_year=="2024","2023-24",cohort_year))

grad_all_subgroups %>% write_csv("data/ACPS_graduation_data_2008-24.csv")


