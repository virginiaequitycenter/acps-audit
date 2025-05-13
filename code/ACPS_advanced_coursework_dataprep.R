##############################################################
# ACPS Student Success Audit - AP/DE Enrollment Rates         
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-03-18
# Summary: A document that creates a group of datasets from the
#          advanced programs enrollment data on VDOE's web page 
##############################################################

## data from VDOE Advanced Programs webpage:
##https://www.doe.virginia.gov/data-policy-funding/data-reports/program-participation-data/advanced-programs
### Pull the following excel spreadsheets:

#2023-2024(xlsx)
#2022-2023(xlsx)
#2021-2022(xlsx)
#2020-2021(xlsx)
#2019-2020(xlsx)


# Need to pull all three sheets from each excel spreadsheet to derive outputs
# Outputs:
#   Students in AP and DE classes by Race
#   Students in AP and DE classes by Economic Advantage
#   Students in AP and DE classes by School

## data from VDOE Enrollment Build-a-Table:
## https://p1pe.doe.virginia.gov/apex/f?p=246:1:108853500518675::::: 
### Query parameters:
# Year(s) = 2019-20 through 2023-24
# Report Level: Division and School
# Select Student Characteristics =
#     Create table with all Racial Categories
#     Grades 10,11, and 12
# Reporting Categories = create a separate table for Disadvantaged 

##############################################################
# Call Libraries
##############################################################

library(readxl)
library(dplyr)
library(tidyverse)
library(janitor)

##############################################################
# Call and Merge AP and DE Datasets
##############################################################

#2019-2020
ap_201920_all <- read_excel("raw_data/ap_2019-20.xlsx", sheet="Adv Programs",range="A4:l2075") %>%
  clean_names() %>% 
  add_column(year=2019) %>%
  add_column(data_level="All") %>%
  add_column(student_group="All Students") %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_201920_race <- read_excel("raw_data/ap_2019-20.xlsx", sheet="Adv Programs by Race",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2019) %>%
  add_column(data_level="Race") %>%
  rename(student_group=race) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)
  
ap_201920_econ <- read_excel("raw_data/ap_2019-20.xlsx", sheet="Adv Programs by Disadvantage",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2019) %>%
  add_column(data_level="Economic Advantage") %>%
  rename(student_group=disadvantage) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_201920 <- merge(x=ap_201920_all,y=ap_201920_race,all.x=TRUE,all.y=TRUE)
ap_201920 <- merge(x=ap_201920,y=ap_201920_econ,all.x=TRUE,all.y=TRUE)

#2020-2021
ap_202021_all <- read_excel("raw_data/ap_2020-21.xlsx", sheet="Adv Programs",range="A4:l2075") %>%
  clean_names() %>% 
  add_column(year=2020) %>%
  add_column(data_level="All") %>%
  add_column(student_group="All Students") %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202021_race <- read_excel("raw_data/ap_2020-21.xlsx", sheet="Adv Programs by Race",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2020) %>%
  add_column(data_level="Race") %>%
  rename(student_group=race) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202021_econ <- read_excel("raw_data/ap_2020-21.xlsx", sheet="Adv Programs by Disadvantage",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2020) %>%
  add_column(data_level="Economic Advantage") %>%
  rename(student_group=disadvantage) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202021 <- merge(x=ap_202021_all,y=ap_202021_race,all.x=TRUE,all.y=TRUE)
ap_202021 <- merge(x=ap_202021,y=ap_202021_econ,all.x=TRUE,all.y=TRUE)

#2021-2022
ap_202122_all <- read_excel("raw_data/ap_2021-22.xlsx", sheet="Adv Programs",range="A4:l2075") %>%
  clean_names() %>% 
  add_column(year=2021) %>%
  add_column(data_level="All") %>%
  add_column(student_group="All Students") %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202122_race <- read_excel("raw_data/ap_2021-22.xlsx", sheet="Adv Programs by Race",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2021) %>%
  add_column(data_level="Race") %>%
  rename(student_group=race) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202122_econ <- read_excel("raw_data/ap_2021-22.xlsx", sheet="Adv Programs by Disadvantage",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2021) %>%
  add_column(data_level="Economic Advantage") %>%
  rename(student_group=disadvantage) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202122 <- merge(x=ap_202122_all,y=ap_202122_race,all.x=TRUE,all.y=TRUE)
ap_202122 <- merge(x=ap_202122,y=ap_202122_econ,all.x=TRUE,all.y=TRUE)

#2022-2023
ap_202223_all <- read_excel("raw_data/ap_2022-23.xlsx", sheet="Adv Programs",range="A4:l2075") %>%
  clean_names() %>% 
  add_column(year=2022) %>%
  add_column(data_level="All") %>%
  add_column(student_group="All Students") %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202223_race <- read_excel("raw_data/ap_2022-23.xlsx", sheet="Adv Programs by Race",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2022) %>%
  add_column(data_level="Race") %>%
  rename(student_group=race) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202223_econ <- read_excel("raw_data/ap_2022-23.xlsx", sheet="Adv Programs by Disadvantage",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2022) %>%
  add_column(data_level="Economic Advantage") %>%
  rename(student_group=disadvantage) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202223 <- merge(x=ap_202223_all,y=ap_202223_race,all.x=TRUE,all.y=TRUE)
ap_202223 <- merge(x=ap_202223,y=ap_202223_econ,all.x=TRUE,all.y=TRUE)

#2023-2024
ap_202324_all <- read_excel("raw_data/ap_2023-24.xlsx", sheet="Adv Programs",range="A4:l2075") %>%
  clean_names() %>% 
  add_column(year=2023) %>%
  add_column(data_level="All") %>%
  add_column(student_group="All Students") %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202324_race <- read_excel("raw_data/ap_2023-24.xlsx", sheet="Adv Programs by Race",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2023) %>%
  add_column(data_level="Race") %>%
  rename(student_group=race) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202324_econ <- read_excel("raw_data/ap_2023-24.xlsx", sheet="Adv Programs by Disadvantage",range="A4:m2075") %>%
  clean_names() %>% 
  add_column(year=2023) %>%
  add_column(data_level="Economic Advantage") %>%
  rename(student_group=disadvantage) %>%
  subset(division_name=="Albemarle County") %>%
  subset(school_number!=890)

ap_202324 <- merge(x=ap_202324_all,y=ap_202324_race,all.x=TRUE,all.y=TRUE)
ap_202324 <- merge(x=ap_202324,y=ap_202324_econ,all.x=TRUE,all.y=TRUE)

ap_allyears <- merge(x=ap_201920,y=ap_202021,all.x=TRUE,all.y=TRUE)
ap_allyears <- merge(x=ap_allyears,y=ap_202122,all.x=TRUE,all.y=TRUE)
ap_allyears <- merge(x=ap_allyears,y=ap_202223,all.x=TRUE,all.y=TRUE)
ap_allyears <- merge(x=ap_allyears,y=ap_202324,all.x=TRUE,all.y=TRUE)

# Change Categories for Years
ap_allyears <- ap_allyears %>% mutate(year=ifelse(year==2019,"2019-20",year))
ap_allyears <- ap_allyears %>% mutate(year=ifelse(year==2020,"2020-21",year))
ap_allyears <- ap_allyears %>% mutate(year=ifelse(year==2021,"2021-22",year))
ap_allyears <- ap_allyears %>% mutate(year=ifelse(year==2022,"2022-23",year))
ap_allyears <- ap_allyears %>% mutate(year=ifelse(year==2023,"2023-24",year))


##############################################################
# Dataset Cleaning
##############################################################

# Rename and Select Columns of Interest
ap_allyears <- ap_allyears %>% rename(ap_stu_num=students_taking_1_or_more_ap_courses,
                                      de_stu_num=students_taking_1_or_more_dual_enrollment_courses_1)
ap_allyears <- ap_allyears %>% select(year,division_name,school_number,school_name,data_level,
                                      student_group,ap_stu_num,de_stu_num)
# Change Group Categories for Student Groups
ap_allyears <- ap_allyears %>% mutate(student_group=
               ifelse(student_group=="Black, not of Hispanic origin","Black",student_group))
ap_allyears <- ap_allyears %>% mutate(student_group=
               ifelse(student_group=="Non-Hispanic, two or more races","Multiracial",student_group))
ap_allyears <- ap_allyears %>% mutate(student_group=
               ifelse(student_group=="White, not of Hispanic origin","White",student_group))
ap_allyears <- ap_allyears %>% mutate(student_group=
               ifelse(student_group=="N","Economically Advantaged",student_group))
ap_allyears <- ap_allyears %>% mutate(student_group=
               ifelse(student_group=="Y","Economically Disadvantaged",student_group))

#Clean NAs and Convert AP/DE to Numeric Type
ap_allyears <- ap_allyears %>% replace(is.na(.), 0)

ap_allyears <- ap_allyears %>% 
  mutate_at(vars("ap_stu_num","de_stu_num"),as.numeric)

##############################################################
# Call and Merge Relevant Enrollment Data
##############################################################

membership_disadv <- read_csv("raw_data/ACPS_membership_econ_1923.csv") %>%
  clean_names() %>%
  add_column(data_level="Economic Advantage") %>%
  mutate(disadvantaged=ifelse(disadvantaged=="Y","Economically Disadvantaged","Economically Advantaged")) %>%
  rename(student_group=disadvantaged)

membership_race <- read_csv("raw_data/ACPS_membership_race_1923.csv") %>%
  clean_names() %>%
  add_column(data_level="Race") %>%
  rename(student_group=race)

membership_all <- read_csv("raw_data/ACPS_membership_all_1923.csv") %>%
  clean_names() %>%
  add_column(data_level="All") %>%
  add_column(student_group="All Students")

membership <- merge(x=membership_disadv,y=membership_race,all.x=TRUE,all.y=TRUE)
membership <- merge(x=membership,y=membership_all,all.x=TRUE,all.y=TRUE)

##############################################################
# Clean Enrollment Data
##############################################################

#Rename and select columns of interest
membership <- membership %>% select("school_year","student_group","school_name",
                                    "grade_count","data_level","level")
membership <- membership %>% rename(year=school_year)

# Change Group Categories for Student Groups
membership <- membership %>% mutate(student_group=
                                        ifelse(student_group=="Black, not of Hispanic origin","Black",student_group))
membership <- membership %>% mutate(student_group=
                                        ifelse(student_group=="Non-Hispanic, two or more races","Multiracial",student_group))
membership <- membership %>% mutate(student_group=
                                        ifelse(student_group=="White, not of Hispanic origin","White",student_group))

membership <- membership %>% mutate(year=ifelse(year=="2019-2020","2019-20",year))
membership <- membership %>% mutate(year=ifelse(year=="2020-2021","2020-21",year))
membership <- membership %>% mutate(year=ifelse(year=="2021-2022","2021-22",year))
membership <- membership %>% mutate(year=ifelse(year=="2022-2023","2022-23",year))
membership <- membership %>% mutate(year=ifelse(year=="2023-2024","2023-24",year))

##############################################################
# Merge Enrollment Data with AP and DE Data for Division
##############################################################
membership_division <- membership %>% subset(level=="Division")
ap_allyears_div <- ap_allyears %>% group_by(student_group,year,data_level) %>% summarise(ap_sum=sum(ap_stu_num),de_sum=sum(de_stu_num))
ap_allyears_div <- merge(x=membership_division,y=ap_allyears_div,all.x=TRUE,all.y=TRUE)
ap_allyears_div <- ap_allyears_div %>% add_column(perc_ap=round((ap_allyears_div$ap_sum/ap_allyears_div$grade_count)*100,2))
ap_allyears_div <- ap_allyears_div %>% add_column(perc_de=round((ap_allyears_div$de_sum/ap_allyears_div$grade_count)*100,2))

colnames(ap_allyears_div) <- c("year","student_group","data_level","school_name","tenth-twelfth_grade_count",
                                "level","sum_ap","sum_de","perc_ap","perc_de")
ap_allyears_div <- ap_allyears_div %>% select(!c("level","school_name"))


ap_allyears_div %>% write_csv("data/ACPS_advanced_coursework_div_data_2019-23.csv")

##############################################################
# Merge Enrollment Data with AP and DE Data for School 
##############################################################
membership_school <- membership %>% subset(level=="School")
ap_allyears_school <- ap_allyears %>% group_by(school_name,student_group,year,data_level) %>% summarise(ap_sum=sum(ap_stu_num),de_sum=sum(de_stu_num))
ap_allyears_school <- merge(x=membership_school,y=ap_allyears_school,all.x=TRUE,all.y=TRUE)
ap_allyears_school <- ap_allyears_school %>% add_column(perc_ap=round((ap_allyears_school$ap_sum/ap_allyears_school$grade_count)*100,2))
ap_allyears_school <- ap_allyears_school %>% add_column(perc_de=round((ap_allyears_school$de_sum/ap_allyears_school$grade_count)*100,2))

colnames(ap_allyears_school) <- c("year","student_group","school_name","data_level","tenth-twelfth_grade_count",
                                  "level","sum_ap","sum_de","perc_ap","perc_de")
ap_allyears_school <- ap_allyears_school %>% select(!"level")

ap_allyears_school %>% write_csv("data/ACPS_advanced_coursework_school_data_2019-23.csv")

