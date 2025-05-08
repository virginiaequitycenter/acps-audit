##############################################################
# ACPS Student Success Audit - IHE Enrollment Rates         
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-03-18
# Summary: A document that creates a group of datasets from the
#          Cohort Graduation Build-a-Table for ACPS Audit
##############################################################

## data from VDOE Postsecondary Enrollment Reports Build-a-Table:
## https://p1pe.doe.virginia.gov/postsec_public/showParamPostsec_publicReport.do?report_id=state-fiscal-stabilization-fund

### Query parameters:

# FGI Cohort Year = 2011 through 2023
# Graduation Rate Type: Four Year Rate
# School Division Results
# Division Name: Albemarle County
# View Excel 

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

postsec_2011 <- read_excel("raw_data/postsec_2011.xlsx", range="A7:J20") %>%
                add_column(year=2011) %>%
                rename(student_group=Subgroup,
                       num_hs_diploma=Number...2,
                       any_ihe_num=Number...3,
                       any_ihe_pct=Percent...4,
                       fourpub_ihe_num=Number...5,
                       fourpub_ihe_pct=Percent...6,
                       fourpri_ihe_num=Number...7,
                       fourpri_ihe_pct=Percent...8,
                       two_ihe_num=Number...9,
                       two_ihe_pct=Percent...10)

postsec_2012 <- read_excel("raw_data/postsec_2012.xlsx", range="A7:J20") %>%
  add_column(year=2012) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2013 <- read_excel("raw_data/postsec_2013.xlsx", range="A7:J20") %>%
  add_column(year=2013) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)
  
postsec_2014 <- read_excel("raw_data/postsec_2014.xlsx", range="A7:J19") %>%
  add_column(year=2014) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2015 <- read_excel("raw_data/postsec_2015.xlsx", range="A7:J18") %>%
  add_column(year=2015) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2016 <- read_excel("raw_data/postsec_2016.xlsx", range="A7:J20") %>%
  add_column(year=2016) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2017 <- read_excel("raw_data/postsec_2017.xlsx", range="A7:J20") %>%
  add_column(year=2017) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2018 <- read_excel("raw_data/postsec_2018.xlsx", range="A7:J19") %>%
  add_column(year=2018) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2019 <- read_excel("raw_data/postsec_2019.xlsx", range="A7:J19") %>%
  add_column(year=2019) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2020 <- read_excel("raw_data/postsec_2020.xlsx", range="A7:J18") %>%
  add_column(year=2020) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2021 <- read_excel("raw_data/postsec_2021.xlsx", range="A7:J18") %>%
  add_column(year=2021) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2022 <- read_excel("raw_data/postsec_2022.xlsx", range="A7:J19") %>%
  add_column(year=2022) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

postsec_2023 <- read_excel("raw_data/postsec_2023.xlsx", range="A7:J19") %>%
  add_column(year=2023) %>%
  rename(student_group=Subgroup,
         num_hs_diploma=Number...2,
         any_ihe_num=Number...3,
         any_ihe_pct=Percent...4,
         fourpub_ihe_num=Number...5,
         fourpub_ihe_pct=Percent...6,
         fourpri_ihe_num=Number...7,
         fourpri_ihe_pct=Percent...8,
         two_ihe_num=Number...9,
         two_ihe_pct=Percent...10)

##############################################################
# Merge Datasets
##############################################################

ihe_allyears <- merge(postsec_2011,postsec_2012, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2013, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2014, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2015, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2016, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2017, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2018, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2019, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2020, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2021, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2022, all.x=TRUE,all.y=TRUE)
ihe_allyears <- merge(ihe_allyears,postsec_2023, all.x=TRUE,all.y=TRUE)

ihe_allyears <- ihe_allyears %>% add_column(data_level="")
ihe_allyears <- ihe_allyears %>% mutate(data_level=
                                        ifelse(student_group=="Economically Disadvantaged",
                                               "Economic Status",data_level))
ihe_allyears <- ihe_allyears %>% mutate(data_level=
                                          ifelse(student_group=="Students with Disabilities",
                                                 "Disability",data_level))
ihe_allyears <- ihe_allyears %>% mutate(data_level=
                                          ifelse(student_group=="Limited English Proficient Students",
                                                 "English Learners",data_level))
ihe_allyears <- ihe_allyears %>% mutate(data_level=
                                          ifelse(student_group=="Male","Gender",data_level))
ihe_allyears <- ihe_allyears %>% mutate(data_level=
                                          ifelse(student_group=="Female","Gender",data_level))
ihe_allyears <- ihe_allyears %>% mutate(data_level=
                                          ifelse(student_group=="All Students","All",data_level))
ihe_allyears <- ihe_allyears %>% mutate(data_level=
                                          ifelse(data_level=="","Race",data_level))


##############################################################
# Merge Datasets
##############################################################

ihe_allyears <- ihe_allyears %>% 
  mutate_at(vars("num_hs_diploma","any_ihe_num","any_ihe_pct","fourpub_ihe_num",
                 "fourpub_ihe_pct","fourpri_ihe_num","fourpri_ihe_pct","two_ihe_num",
                 "two_ihe_pct","year"),as.numeric)

ihe_allyears <- ihe_allyears %>% add_column(four_ihe_num=0)
ihe_allyears <- ihe_allyears %>% mutate(four_ihe_num=fourpri_ihe_num+fourpub_ihe_num)

ihe_allyears <- ihe_allyears %>% add_column(four_ihe_pct=0)
ihe_allyears <- ihe_allyears %>% mutate(four_ihe_pct=fourpri_ihe_pct+fourpub_ihe_pct)

ihe_allyears <- ihe_allyears %>% add_column(short_year=0)
ihe_allyears <- ihe_allyears %>% mutate(short_year=year-2000)

ihe_allyears <- ihe_allyears %>% select("year","short_year","data_level","student_group",
                                        "num_hs_diploma","any_ihe_num","any_ihe_pct",
                                        "fourpub_ihe_num","fourpub_ihe_pct","fourpri_ihe_num",
                                        "fourpri_ihe_pct","four_ihe_num","four_ihe_pct",
                                        "two_ihe_num","two_ihe_pct")


ihe_allyears %>% write_csv("data/ACPS_higher_ed_enrollment_data_2008-24.csv")
