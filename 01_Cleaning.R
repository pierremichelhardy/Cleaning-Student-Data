#
#  \'__                         
#  \   '__                  
#  \      '__               
#  \  _    _ '__            
#  \ \ \  \ \   '__         
#  \ \ \  \ \      '__        _____ _____ _____ _____ ____            _____                           ___
#  \ \ \  \ \       __'      | ____| ____\_   _\  __ \__  \          | ____\                         | _ \
#  \ \ \__\ \    __'   __'\  \ (___\ (___  \ \ \ \__) \ | |  ______  \ \  __ _ __ ___  _   _ _ __   \ (_) \ 
#  \  \____/  __'   __'   \  \___ \\___ \  \ \ \  ___| | |  \______\ \ \ \_ \ '__| _ \\ \ \ \ '_ \   > _ < 
#  \       __'   __'      \  ____) \___) \_\ \_\ \    | |_           \ \__\ \ \ \ (_) \ \_\ \ \_) \ \ (_) \
#  \    __'   __' __  __  \  \_____|_____|\____\_\    \____\          \_____\_\  \___| \__,_\ .__|   \____|
#  \,__'   __'   \  \/  \ \                                                                 \ \            
#       __'      \ \  | \ \                                                                 \_\            
#      '__       \ \\|\ \ \  Crijns, Nardie
#         '__    \ \  \ \ \  Hardy, Pierre
#            '__ \_\  \_\ \  Lalisang, Robbert
#   		        '__       \  Mauer, Christoph
#                  '__    \  Thierfelder, Sebastian
#                     '__,\ 
# 

###########################################
###            CLEANING CODE            ###
###########################################

# This is the cleaning code for Group 8's Smart Service Innovation Project II
# The cleaning code wrangles the student data required for our group's models, in partial fulfillment of the 
# requirements of our master's program in Business Intelligence and Smart Services. 

# Cleaning was specifically made by Robbert Lalisang and Pierre Hardy. 
# Because of this, the code can be discerned by 2 chunks. First is by Robbert, second is by Pierre
# The work of these two hunks have been combined into this one code. 

# REQUIRED DATASET TO BE IN THE SAME WORKING DIRECTORY:
# BISS data - Bachelors - Student version.xlsx
# BISS data - Bachelors 2012 - Student version.xlsx
# BISS data - Masters - Student version.xlsx
# BISS data - Leads - Student version.xlsx
# BISS data - Bachelors Admissions 2012 - Student version.xlsx
# BISS data - Masters 2018 - Student version.xlsx
# BISS data - BSc Graduates - Student version.xlsx
# BISS data - Bachelors Academic work 2012 - Student version.xlsx
# Ranking EBE - Student version.xlsx
# Ranking IB - Student version.xlsx
# BISS data - Pre-education - Student version.xlsx
# Specialisations - Student version.xlsx

# !!! PLEASE MAKE SURE TO SET WORKING DIRECTORY RIGHT BEFORE RUNNING. !!!
# !!! PLEASE MAKE SURE TO INCLUDE ALL THE DATASETS WITHIN THE WORKING DIRECTORY BEFORE RUNNING !!!

# dir <- choose.dir() # Choose directory
# setwd(dir) # set working directory

###########################################
###              PACKAGES               ###
###########################################

# install.packages(c('readr','dplyr', 'tidyr', 'dplyr', 'reshape2', 'lubridate', 'caret', 'xlsx',
#                    'ggplot2','Amelia','randomForest', 'readxl', 'rlang', 'R6Frame',
#                    "data.table", 'csv'))

library(readr)
library(dplyr)
library(tidyr)
library(reshape2)
library(lubridate)
library(caret)
library(xlsx)
library(ggplot2)
library(Amelia)
library(randomForest)
library(readxl)
library(rlang)
library(data.table)
library(csv)


###########################################
###               DATA                  ###
###########################################

# Set working directory so that everything runs from the original files
# setwd("~/BISS Master/Smart Service Innovation Project II/Data/Source")

setwd(dir)


bscgrades <- read_excel("BISS data - Bachelors - Student version.xlsx", sheet = "Academic Work")
bscgrades2012 <- read_excel("BISS data - Bachelors Academic work 2012 - Student version.xlsx")
brochure <- read_excel('BISS data - Leads - Student version.xlsx', sheet = 'Brochures')
ranking_EBE <- read_excel('Ranking EBE - Student version.xlsx')
ranking_IB <- read_excel('Ranking IB - Student version.xlsx')
pre_educ <- read_excel('BISS data - Pre-education - Student version.xlsx')
bachelor_merge <- read_excel('BISS data - Bachelors - Student version.xlsx', sheet = 'Master')
bachelor_merge_2012 <- read_excel("BISS data - Bachelors 2012 - Student version.xlsx", sheet = 'Master')
master_merge <- read_excel('BISS data - Masters - Student version.xlsx', sheet = 'Master')
master_merge_2018 <- read_excel('BISS data - Masters 2018 - Student version.xlsx', sheet = 'Master')

admissions <- read_excel(path="BISS data - Bachelors - Student version.xlsx", sheet = "Admissions")
extra.admissions <- read_excel(path="BISS data - Bachelors Admissions 2012 - Student version.xlsx", sheet="Admission")
registrations <- read_excel(path="BISS data - Bachelors - Student version.xlsx", sheet = "Registrations")
extra.registrations <- read_excel(path="BISS data - Bachelors 2012 - Student version.xlsx", sheet="Registrations")
masters <- read_excel(path="BISS data - Masters - Student version.xlsx", sheet="Registrations")
extra.masters <- read_excel(path="BISS data - Masters 2018 - Student version.xlsx", sheet = "Registrations")
graduates <- read_excel(path="BISS data - BSc Graduates - Student version.xlsx")
admissions.master <- read_excel(path="BISS data - Masters - Student version.xlsx", sheet="Admissions")
events <- read_excel(path="BISS data - Leads - Student version.xlsx", sheet = "Events")
special <- read_excel(path="Specialisations - Student version.xlsx")
master.grades <- read_excel(path="BISS data - Masters - Student version.xlsx", sheet="Academic Work")


###########################################
###  DATA WRANGLING/VARIABLE CREATION   ###
###########################################

######################################################################################################################
#         Robbert Lalisang's Part        
######################################################################################################################

# (set appropriately)

# For the bachelor academic work
colnames(bscgrades) # keep column 1, 2, 3, 4, 6, 7, 8, 9, 10, 11
# Drop column 'Module Booking Reason (Description' & 'Appraiser ID (Pseudo)'
dim(bscgrades) # check rows and columns

# Add 2012 grades that were provided later
dim(bscgrades2012) # check rows and columns of 2012 grades

# Rowbind the 2012 grades to the total bscgrades
bscgrades <- rbind(bscgrades, bscgrades2012)
dim(bscgrades)

bscgrades[,c(5, 12)] <- NULL # Drop columns
head(bscgrades)
names(bscgrades) <- c('program', 'period', 'appraisal_type', 'attempts', 'credits'
                      ,'std_number','course_code', 'year', 'grade', 'date')
head(bscgrades)
nrow(bscgrades)

# Some comments while inspecting the data:
# - appraisal type 0 (has no grade but gives credits), consists of exemptions for courses

# Remove duplicate rows (with absolutely the same information; 
# same grade, same course, same date)
bscgrades <- distinct(bscgrades)
nrow(bscgrades) # 130 rows removed

# Inspect data
summary(bscgrades)

# Replace period 100, 200, 300, 400, 500, 600 with 1,2,3,4,5,6
unique(bscgrades$period) # check factors in grade

replace_period <- c(1,2,3,4,5,6)

for (i in c(100, 200, 300, 400, 500, 600)) {
  bscgrades$period <- 
    replace(bscgrades$period, bscgrades$period == i, replace_period[i/100])
}

unique(bscgrades$period)

# For the modeling we decided to only work with students for which we have
# a complete BSc record. We did this by adding all the received credits in the bachelor
# and this should be equal or above 180 credits.


#################################################################################
# Filter for students with complete bachelor
#################################################################################

## Underneath you can find the filter we used before (in comments), but we updated 
# this to add appraisal type 0, because this has exemptions for courses.

# filter_graduate <- bscgrades %>%
#   group_by(std_number) %>%
#   filter(appraisal_type == 10) %>%
#   filter(grade %in% c('A', 'B', 'C', 'PASS', 'EXCELLENT')) %>%
#   mutate(total_credit = sum(credits, na.rm = TRUE)) %>%
#   mutate(full_credits_bsc = ifelse(total_credit >= 180, 1, 0)) %>%
#   filter(full_credits_bsc == 1)

# filter_std <- unique(filter_graduate$std_number) # create vector with unique std_ids
# length(filter_std) # 1815 students with at least 180 credits

filter_graduate_1 <- bscgrades %>% # create new df with appraisal type 0 added
  group_by(std_number) %>%
  filter((appraisal_type == 10 & grade %in% c('A', 'B', 'C', 'PASS', 'EXCELLENT'))|
           appraisal_type == 0) %>% # also add appraisal type 0 (exemptions)
  mutate(total_credit = sum(credits, na.rm = TRUE)) %>%
  mutate(full_credits_bsc = ifelse(total_credit >= 180, 1, 0)) %>%
  filter(full_credits_bsc == 1) 

filter_std_1 <- unique(filter_graduate_1$std_number) # create vector with unique std_ids
length(filter_std_1) # in this way 1882 students with at least 180 credits


#################################################################################
# Filter for relevant grades only
#################################################################################

# Every course for each student has multiple rows with multiple appraisal
# types. After discussing with Nico and having a look at the grades data we decided 
# to keep appraisal type 10 for each course. Reasons for this are:
# - Appraisal type 10 has grades mostly on a scale from A-F (and not just pass/fail)
# - Appraisal type 10 is a composite grade for the total course and does not consist
#   of small parts, like participation only.

# Appraisal type 7055 has mostly PASS/FAIL only for each course. However, some courses 
# don't have a appraisal type 10 (A - F), but do have appraisal type 7055 (PASS/FAIL).

# So in the end we took appraisal type 10 if present (prefered scale of A-F). If, however,
# appraisal type 10 was not present for a course and appraisal type 7055 (PASS/FAIL) was
# present, we also wanted to keep that grade (to minimize the loss of information). 

# filter(course_code == 'ABC1245') --> semester abroad. everybody passed.

bscgrades <- bscgrades[c(bscgrades$grade != ''),] # remove empty grade columns: useless

# Filter for appropriate grades
graduate_bsc_count <- bscgrades %>%
  # group_by
  group_by(std_number) %>%
  # only include students who completed their bsc
  filter(std_number %in% filter_std_1) %>% 
  filter(appraisal_type == 10 | appraisal_type == 7055) %>% 
  group_by(std_number, course_code) %>%
  # arrange/sort variables
  # descending appraisal type because of distinct function later (keeps only first row)
  arrange(std_number, year, period, course_code, desc(appraisal_type), date) %>% 
  # remove all PASS for courses for which there is also a grade (A-C)
  filter(!(any(grade %in% c('A', 'B', 'C')) & grade == 'PASS')) %>%
  # remove all FAILS for courses for which there is also a grade (D-F)
  filter(!(any(grade %in% c('D', 'E', 'F')) & grade == 'FAIL')) %>% 
  # remove all NO GRADE for which there is also a grade
  filter(!(any(grade %in% c('A', 'B', 'C', 'D', 'E', 'F', 'PASS', 'FAIL', 'EXCELLENT')) 
           & grade == 'NO GRADE')) %>%
  # remove all duplicates in every column except for appraisal type and credits 
  distinct(program, period, std_number, course_code, year, grade, date, .keep_all = TRUE) %>% 
  #filter(n()>1) %>% # show courses with more than one grade
  # manually check courses for which we have both appraisal type 10
  #filter((any(appraisal_type == 7055) & appraisal_type == 10)| 
  #(any(appraisal_type == 10) & appraisal_type == 7055)) %>% 
  # filter our 170 cases of courses for which we have a grade in appraisal type 10 that is 
  # different from one of the grades in appraisal type 7055 appraisal type 10
  filter(!(any(appraisal_type == 7055) & appraisal_type == 10)) 


# Now we have a dataset with the appropriate students and the appropriate grades;
# so we can start with feature extraction.

#################################################################################
# Construct max number resits for a course and total number of resits variable
#################################################################################

max_resits <- graduate_bsc_count %>%
  group_by(std_number, course_code) %>%
  mutate(resits_per_course = row_number() -1) %>%
  group_by(std_number) %>%
  mutate(max_resits_for_course = max(resits_per_course)) %>%
  select(std_number, max_resits_for_course) %>%
  distinct(std_number, .keep_all = TRUE) 

total_resits <- graduate_bsc_count %>%
  group_by(std_number, course_code) %>%
  mutate(resits_per_course = row_number() -1) %>%
  filter(resits_per_course == max(resits_per_course)) %>%
  group_by(std_number) %>%
  mutate(total_resits = sum(resits_per_course)) %>%
  select(std_number, total_resits) %>%
  distinct(std_number, .keep_all = TRUE)


# reading code line by line can be quite burdensome. A cat every now and then is well deserved. This one is Nardie.  
#  
#           __..--''``---....___   _..._    __
#      _.-'    .-/";  `        ``<._  ``.''_ `.
#  _.-' _..--.'_    \                    `( ) )
# (_..-'    (< _     ;_..__               ; `'
#            `-._,_)'      ``--...____..-'  


#################################################################################
#################################################################################
# First we will construct features per course
#################################################################################
#################################################################################

#################################################################################
# Construct count per course dataset
#################################################################################

# number of grades per course
bscgrades_wide_count <- dcast(graduate_bsc_count, std_number ~ course_code, 
                              value.var = 'grade', fun.aggregate=function(x) length(x))
head(bscgrades_wide_count)

# check
sum(as.numeric(bscgrades_wide_count == 1)) # 45674
sum(as.numeric(bscgrades_wide_count == 2)) # 3449
sum(as.numeric(bscgrades_wide_count == 3)) # 946
sum(as.numeric(bscgrades_wide_count == 4)) # 11 
sum(as.numeric(bscgrades_wide_count == 5)) # 285
sum(as.numeric(bscgrades_wide_count == 6)) # 26
sum(as.numeric(bscgrades_wide_count == 7)) # 2
sum(as.numeric(bscgrades_wide_count == 8)) # 1
sum(as.numeric(bscgrades_wide_count == 9)) # 0


#################################################################################
# Construct grades (min, max, mean) per course dataset
#################################################################################

graduate_bsc_grades <- bscgrades %>%
  group_by(std_number) %>%
  # only include students who completed their bsc
  filter(std_number %in% filter_std_1) %>% 
  filter(appraisal_type == 10 | appraisal_type == 7055) %>% 
  group_by(std_number, course_code) %>%
  # arrange/sort variables
  # descending appraisal type because of distinct function later (keeps only first row)
  arrange(std_number, year, period, course_code, desc(appraisal_type), date) %>% 
  # remove all PASS for courses for which there is also a grade (A-C)
  filter(!(any(grade %in% c('A', 'B', 'C')) & grade == 'PASS')) %>%
  # remove all FAILS for courses for which there is also a grade (D-F)
  filter(!(any(grade %in% c('D', 'E', 'F')) & grade == 'FAIL')) %>% 
  # remove all NO GRADE for which there is also a grade
  filter(!(any(grade %in% c('A', 'B', 'C', 'D', 'E', 'F', 'PASS', 'FAIL', 'EXCELLENT')) 
           & grade == 'NO GRADE')) %>%
  # remove all duplicates in every column except for appraisal type and credits 
  distinct(program, period, std_number, course_code, year, grade, date, .keep_all = TRUE) 
# keep 170 cases of courses for which we have a grade in appraisal type 10 that is different from 
# one of the grades in appraisal type 7055 appraisal type 10. We removed these for the count.


#################################################################################
# Construct highest grade per course dataset
#################################################################################

graduate_bsc2 <- as.data.frame(graduate_bsc_grades) # create copy and convert to df

# add order to factor grades to extract the highest grade for each course
graduate_bsc2[,'grade'] <- ordered(graduate_bsc2[,'grade'], levels = 
                                     c('NO GRADE', 'FAIL', 'F', 'E', 'D', 'PASS', 
                                       'C', 'B', 'EXCELLENT', 'A'))
summary(graduate_bsc2[,'grade'])
min(graduate_bsc2[,'grade']) # min is no grade
max(graduate_bsc2[,'grade']) # max is A

# generate highest grade per course
bscgrades_wide_max <- dcast(graduate_bsc2, std_number ~ course_code, value.var = 'grade', 
                            fun.aggregate=function(x) as.character(max(unique(x))))

head(bscgrades_wide_max)


#################################################################################
# MISSMAP
#################################################################################

# Remove columns with more than 90% NA
bscgrades_wide_max1 <- bscgrades_wide_max[, -which(colMeans(is.na(bscgrades_wide_max)) > 0.9)]

missmap(bscgrades_wide_max1, y.labels = NULL, y.at = NULL, 
        main = 'Max, all programs, less than 90% NA')

bscgrades_wide_max2 <- bscgrades_wide_max[, -which(colMeans(is.na(bscgrades_wide_max)) > 0.3)]

missmap(bscgrades_wide_max2, y.labels = NULL, y.at = NULL, 
        main = 'Max, all programs, less than 30% NA')


#################################################################################
# Construct lowest grade per course dataset
#################################################################################

graduate_bsc3 <- graduate_bsc2
# change order of factor grades to extract the lowest grade for each course
graduate_bsc3[,'grade'] <- ordered(graduate_bsc3[,'grade'], levels = 
                                     c('F', 'E', 'D', 'FAIL',  
                                       'C', 'B', 'A', 'PASS', 'EXCELLENT', 'NO GRADE'))
summary(graduate_bsc3[,'grade'])
min(graduate_bsc3[,'grade']) # min is F
max(graduate_bsc3[,'grade']) # max is NO GRADE --> is this correct???

# generate lowest grade per course
bscgrades_wide_min <- dcast(graduate_bsc3, std_number ~ course_code, value.var = 'grade', 
                            fun.aggregate=function(x) as.character(min(unique(x))))

head(bscgrades_wide_min)


#################################################################################
# Missingness map
#################################################################################

# First remove courses that less than 10% took (more than 90% NA)
# Otherwise the missingness map gets too confusing to interpret

bscgrades_wide_min1 <- 
  bscgrades_wide_min[, -which(colMeans(is.na(bscgrades_wide_min)) > 0.9)]

missmap(bscgrades_wide_min1, y.labels = NULL, y.at = NULL, 
        main = 'Both BSc programs, at least 10% observed grades')

# Remove columns with more than 30% NA
bscgrades_wide_min2 <- 
  bscgrades_wide_min[, -which(colMeans(is.na(bscgrades_wide_min)) > 0.3)]

missmap(bscgrades_wide_min2, y.labels = NULL, y.at = NULL, 
        main = 'Both BSc programs, at least 70% observed grades')


#################################################################################
# Make dataset for different BSc programs
#################################################################################

length(unique(graduate_bsc3$std_number)) # 1882 total students in dataset

program2504 <- graduate_bsc3 %>%
  filter(program == 2504) 

length(unique(program2504$std_number)) # 1354 students with grades in program 2504

program2501 <- graduate_bsc3 %>%
  filter(program == 2501) 

length(unique(program2501$std_number)) # 557 students with grades in program 2501 (some double programs)


#################################################################################
# Create columns for program
#################################################################################
programs <- aggregate(program ~ std_number, graduate_bsc3, function(x) toString(unique(x)))

# add column with number of programs
programs_count <- aggregate(program ~ std_number, graduate_bsc3, function(x) length(unique(x)))


#################################################################################
# Check if missing courses are due to different BSc program
#################################################################################
bscgrades_wide_min_program <- left_join(bscgrades_wide_min, programs, by = 'std_number')

## Remove columns with more than 90% NA
bscgrades_wide_min_program1 <- 
  bscgrades_wide_min_program[, -which(colMeans(is.na(bscgrades_wide_min_program)) > 0.9)]

missmap(bscgrades_wide_min_program1, y.labels = NULL, y.at = NULL, 
        main = 'Min, all programs, less than 90% NA')

bscgrades_wide_min_program1 %>%
  filter(program == 2504) %>%
  missmap(y.labels = NULL, y.at = NULL,
          main = 'BSc program 2504, at least 10% observed grades')

bscgrades_wide_min_program1 %>%
  filter(program == 2501) %>%
  missmap(y.labels = NULL, y.at = NULL,
          main = 'BSc program 2501, at least 10% observed grades')


## Remove columns with more than 30% NA
bscgrades_wide_min_program2 <- 
  bscgrades_wide_min_program[, -which(colMeans(is.na(bscgrades_wide_min_program)) > 0.3)]

missmap(bscgrades_wide_min_program2, y.labels = NULL, y.at = NULL,
        main = 'Min, all programs, less than 30% NA')

bscgrades_wide_min_program2 %>%
  filter(program == 2504) %>%
  missmap(y.labels = NULL, y.at = NULL, 
          main = 'Min, program 2504, less than 30% NA')

bscgrades_wide_min_program2 %>%
  filter(program == 2501) %>%
  missmap(y.labels = NULL, y.at = NULL,
          main = 'Min, program 2501, less than 30% NA')

# THE MISSINGS ARE CLEARLY MOSTLY DUE TO DIFFERENCE IN PROGRAM
# We can also see specialisations particularly in program 2501


#################################################################################
# Create variables for number of A, B, C, D, E, F and so on
#################################################################################

graduate_bsc_grades1 <- bscgrades %>%
  group_by(std_number) %>%
  # only include students who completed their bsc
  filter(std_number %in% filter_std_1) %>% 
  filter(appraisal_type == 10 | appraisal_type == 7055) %>%
  group_by(std_number, course_code) %>%
  arrange(std_number, year, period, course_code, desc(appraisal_type), date) %>%
  # remove all PASS for courses for which there is also a grade (A-C)
  filter(!(any(grade %in% c('A', 'B', 'C')) & grade == 'PASS')) %>% 
  # remove all FAILS for courses for which there is also a grade (D-F)
  filter(!(any(grade %in% c('D', 'E', 'F')) & grade == 'FAIL')) %>% 
  # remove all NO GRADE for which there is also a grade
  filter(!(any(grade %in% c('A', 'B', 'C', 'D', 'E', 'F', 'PASS', 'FAIL', 'EXCELLENT')) 
           & grade == 'NO GRADE')) %>%
  # remove all duplicates in every column except for appraisal type and credits
  distinct(program, period, std_number, course_code, year, grade, date, .keep_all = TRUE) %>% 
  filter(!(any(appraisal_type == 7055) & (appraisal_type == 10 & grade %in% 
                                            c('A', 'B', 'C', 'D', 'E'))))
# filter all 122 cases of courses for which we have a grade in appraisal type 10 that is 
# different from one of the grades in appraisal type 7055. We only remove the ABCDE grades 
# because FAIL has shown to be relevant in manually checking these cases 
# (also there was one case where F was actually correct also) 

graduate_bsc_1 <- as.data.frame(graduate_bsc_grades1)

# Add count for each grade
graduate_bsc_1 <- graduate_bsc_1 %>%
  group_by(std_number) %>%
  add_tally(grade == 'A', name = 'count_A') %>%
  add_tally(grade == 'B', name = 'count_B') %>%
  add_tally(grade == 'C', name = 'count_C') %>%
  add_tally(grade == 'D', name = 'count_D') %>%
  add_tally(grade == 'E', name = 'count_E') %>%
  add_tally(grade == 'F', name = 'count_F') %>%
  add_tally(grade == 'EXCELLENT', name = 'count_EXCELLENT') %>%
  add_tally(grade == 'PASS', name = 'count_PASS') %>%
  add_tally(grade == 'FAIL', name = 'count_FAIL') %>%
  add_tally(grade == 'NO GRADE', name = 'count_NO_GRADE')

colnames(graduate_bsc_1)

# no students with a grade E, only 1 student with grade Excellent 


# reading code line by line can be quite burdensome. A cat every now and then is well deserved. This one is Pierre.
#
#                          _
#                         | \
#                         | |
#                         | |
#    |\                   | |
#   /, ~\                / /
#  X     `-.....-------./ /
#   ~-. ~  ~              |
#      \             /    |
#       \  /_     ___\   /
#       | /\ ~~~~~   \ |
#       | | \        || |
#       | |\ \       || )
#      (_/ (_/      ((_/  


#################################################################################
# Create variable with number of years with grades 
#################################################################################

no_of_years_grades <- aggregate(year ~ std_number, graduate_bsc_1, function(x) length(unique(x)))

# check
length(no_of_years_grades[no_of_years_grades == 1])/nrow(no_of_years_grades) # 0% only grades for 1 year
length(no_of_years_grades[no_of_years_grades == 2])/nrow(no_of_years_grades) # 0% only for 2 years
length(no_of_years_grades[no_of_years_grades == 3])/nrow(no_of_years_grades) # 77% for 3 years
length(no_of_years_grades[no_of_years_grades == 4])/nrow(no_of_years_grades) # 19 % for 4 years
length(no_of_years_grades[no_of_years_grades == 5])/nrow(no_of_years_grades) # 2% for 5 years
length(no_of_years_grades[no_of_years_grades == 6])/nrow(no_of_years_grades) # 0% for 6 years
length(no_of_years_grades[no_of_years_grades == 7])/nrow(no_of_years_grades) # 0% for 7 years

# This output makes sense. Since we only include people whom completed their bsc, everyone
# should at least have 3 years with grades. Some people extend their bsc with one or 2 years.


#################################################################################
# Convert grades to numbers
#################################################################################

# Replace grades with numbers. We replace the following grades with the following
# numbers:
# - A = 9
# - B = 7.5
# - C = 6
# - D = 5 (insufficient)
# - E = 4
# - F = 3

replace_grade <- c(9, 7.5, 6, 5, 4, 3) # scale OK? 
grade <- c('A', 'B', 'C', 'D', 'E', 'F')

list_grades <- list() # create empty list

# convert grade column to character
graduate_bsc5 <- graduate_bsc_1
graduate_bsc5$grade <- as.character(graduate_bsc5$grade)

# Remove PASS, FAIL, EXCELLENT, NO GRADE: we can retrieve this information also from the other grades
graduate_bsc5$grade_converted <- ifelse(graduate_bsc5$grade %in% c('PASS', 'FAIL', 'EXCELLENT', 
                                                                   'NO GRADE'), NA, 
                                        graduate_bsc5$grade)

for (i in c(1:6)) {
  graduate_bsc5$grade_converted <- 
    replace(graduate_bsc5$grade_converted, graduate_bsc5$grade_converted == grade[i], 
            replace_grade[i])
}

# convert grades back to numeric
graduate_bsc5$grade_converted <- as.numeric(graduate_bsc5$grade_converted)

summary(graduate_bsc5$grade_converted) # 4547 NA's because of PASS/FAIL/EXCELLENT/NO GRADE


#################################################################################
# Construct mean grade per year, mean grade per program
#################################################################################

# per student (can also do per student + program)
mean_grade_per_year <- dcast(graduate_bsc5, std_number ~ year, value.var = 'grade_converted', 
                             fun.aggregate=function(x) mean(x, na.rm = TRUE))

mean_grade_per_program <- dcast(graduate_bsc5, std_number ~ program, 
                                value.var = 'grade_converted', 
                                fun.aggregate=function(x) mean(x, na.rm = TRUE))

# pay attention: some students did 2 bsc programs
nrow(mean_grade_per_program)
nrow(mean_grade_per_year) 


# Create new dataframe for mean_per_year for year1,year2,year3 and so on          
mean_grade_per_year1 <- mean_grade_per_year
head(mean_grade_per_year1)


for (i in 1:nrow(mean_grade_per_year1)) {
  year1 <- which(!is.na(mean_grade_per_year1[i,2:7]))[1]
  year2 <- which(!is.na(mean_grade_per_year1[i,2:7]))[2]
  year3 <- which(!is.na(mean_grade_per_year1[i,2:7]))[3]
  year4 <- which(!is.na(mean_grade_per_year1[i,2:7]))[4]
  year5 <- which(!is.na(mean_grade_per_year1[i,2:7]))[5]
  year6 <- which(!is.na(mean_grade_per_year1[i,2:7]))[6]
  if (length(which(!is.na(mean_grade_per_year1[i,2:7]))) == 1) {
    mean_grade_per_year1[i, 'year1'] <- mean_grade_per_year1[i, year1 + 1] # +2 for omitted columns
    mean_grade_per_year1[i, 'year2'] <- NA
    mean_grade_per_year1[i, 'year3'] <- NA
    mean_grade_per_year1[i, 'year4'] <- NA
    mean_grade_per_year1[i, 'year5'] <- NA
    mean_grade_per_year1[i, 'year6'] <- NA
  } else if (length(which(!is.na(mean_grade_per_year1[i,2:7]))) == 2) {
    mean_grade_per_year1[i, 'year1'] <- mean_grade_per_year1[i, year1 + 1]
    mean_grade_per_year1[i, 'year2'] <- mean_grade_per_year1[i, year2 + 1]
    mean_grade_per_year1[i, 'year3'] <- NA
    mean_grade_per_year1[i, 'year4'] <- NA
    mean_grade_per_year1[i, 'year5'] <- NA
    mean_grade_per_year1[i, 'year6'] <- NA
  } else if (length(which(!is.na(mean_grade_per_year1[i,2:7]))) == 3) {
    mean_grade_per_year1[i, 'year1'] <- mean_grade_per_year1[i, year1 + 1]
    mean_grade_per_year1[i, 'year2'] <- mean_grade_per_year1[i, year2 + 1]
    mean_grade_per_year1[i, 'year3'] <- mean_grade_per_year1[i, year3 + 1]
    mean_grade_per_year1[i, 'year4'] <- NA
    mean_grade_per_year1[i, 'year5'] <- NA
    mean_grade_per_year1[i, 'year6'] <- NA
  } else if (length(which(!is.na(mean_grade_per_year1[i,2:7]))) == 4) {
    mean_grade_per_year1[i, 'year1'] <- mean_grade_per_year1[i, year1 + 1]
    mean_grade_per_year1[i, 'year2'] <- mean_grade_per_year1[i, year2 + 1]
    mean_grade_per_year1[i, 'year3'] <- mean_grade_per_year1[i, year3 + 1]
    mean_grade_per_year1[i, 'year4'] <- mean_grade_per_year1[i, year4 + 1]
    mean_grade_per_year1[i, 'year5'] <- NA 
    mean_grade_per_year1[i, 'year6'] <- NA
  } else if (length(which(!is.na(mean_grade_per_year1[i,2:7]))) == 5) {
    mean_grade_per_year1[i, 'year1'] <- mean_grade_per_year1[i, year1 + 1]
    mean_grade_per_year1[i, 'year2'] <- mean_grade_per_year1[i, year2 + 1]
    mean_grade_per_year1[i, 'year3'] <- mean_grade_per_year1[i, year3 + 1]
    mean_grade_per_year1[i, 'year4'] <- mean_grade_per_year1[i, year4 + 1]
    mean_grade_per_year1[i, 'year5'] <- mean_grade_per_year1[i, year5 + 1]
    mean_grade_per_year1[i, 'year6'] <- NA
  } else if (length(which(!is.na(mean_grade_per_year1[i,2:7]))) == 6) {
    mean_grade_per_year1[i, 'year1'] <- mean_grade_per_year1[i, year1 + 1]
    mean_grade_per_year1[i, 'year2'] <- mean_grade_per_year1[i, year2 + 1]
    mean_grade_per_year1[i, 'year3'] <- mean_grade_per_year1[i, year3 + 1]
    mean_grade_per_year1[i, 'year4'] <- mean_grade_per_year1[i, year4 + 1]
    mean_grade_per_year1[i, 'year5'] <- mean_grade_per_year1[i, year5 + 1]
    mean_grade_per_year1[i, 'year6'] <- mean_grade_per_year1[i, year6 + 1]
  } else {
    mean_grade_per_year1[i, 'year1'] <- NA
    mean_grade_per_year1[i, 'year2'] <- NA
    mean_grade_per_year1[i, 'year3'] <- NA
    mean_grade_per_year1[i, 'year4'] <- NA
    mean_grade_per_year1[i, 'year5'] <- NA
    mean_grade_per_year1[i, 'year6'] <- NA
  } 
}

nrow(mean_grade_per_year1) # 1882 students

# We now have the mean grade per year (2012-2017) and the mean grade per bsc year (year 1-6)
head(mean_grade_per_year1) 

# We also have the mean grade per program 
head(mean_grade_per_program)


#################################################################################
# Create binary variable for grades more than 3 years 
# (proxy for bachelor took more than 3 years)
#################################################################################

mean_grade_per_year1 <- mean_grade_per_year1 %>%
  mutate(more_than_three_years = ifelse(!is.na(year4), 1,0)) %>%
  mutate(more_than_four_years = ifelse(!is.na(year5), 1,0)) %>%
  mutate(more_than_five_years = ifelse(!is.na(year6), 1,0))


#################################################################################
# Create variable with number of grades, max & min grade, std
#################################################################################

graduate_bsc5 <- graduate_bsc5 %>%
  group_by(std_number) %>%
  mutate(no_of_grades=n()) %>%
  mutate(max_attempts = max(attempts)) %>%
  mutate(mean_grade = mean(grade_converted, na.rm = TRUE)) %>%
  mutate(highest_grade = max(grade_converted, na.rm = TRUE)) %>%
  mutate(lowest_grade = min(grade_converted, na.rm = TRUE)) %>%
  mutate(std_all_grades = sd(grade_converted, na.rm = TRUE))


#################################################################################
# Variable with mean of first 50% of grades and variable with mean of last 
# 50% of grades variable with the difference between the two
#################################################################################

# Create dataframe with first 50% of grades and take mean
graduate_bsc5_first <- graduate_bsc5 %>%
  group_by(std_number) %>%
  arrange(year, period, .by_group = TRUE) %>% # sort by year and period within groups
  filter(!is.na(grade_converted)) %>%
  mutate(no_of_ABCDEF_grades = n()) %>%
  # ceiling function because when using round we'd lose some students because round(0.5) = 0
  filter(row_number() <= as.numeric(ceiling(no_of_ABCDEF_grades/2))) %>% 
  mutate(mean_first_half = mean(grade_converted, na.rm = TRUE))


# Create dataframe with last 50% of grades and take mean
graduate_bsc5_last <- graduate_bsc5 %>%
  group_by(std_number) %>%
  arrange(year, period, .by_group = TRUE) %>% # sort by year and period within groups
  filter(!is.na(grade_converted)) %>%
  mutate(no_of_ABCDEF_grades = n()) %>%
  filter(row_number() > as.numeric(round(no_of_ABCDEF_grades/2))) %>%
  mutate(mean_last_half = mean(grade_converted, na.rm = TRUE))


#################################################################################
# Create variable with number of PASS, number of FAIL
#################################################################################

graduate_bsc5$PASSFAIL <- ifelse(graduate_bsc5$grade %in% 
                                   c('A', 'B', 'C', 'EXCELLENT', 'PASS'), 'PASS', 'FAIL')
graduate_bsc6 <- graduate_bsc5 %>%
  group_by(std_number) %>%
  add_tally(PASSFAIL == 'PASS', name = 'PASS') %>%
  add_tally(PASSFAIL == 'FAIL', name = 'FAIL') 


# reading code line by line can be quite burdensome. A cat every now and then is well deserved. This one is Robbert.
#
#   ("`-''-/").___..--''"`-._
#    `6_ 6  )   `-.  (     ).`-.__.`)
#    (_Y_.)'  ._   )  `._ `. ``-..-'
#  _..`--'_..-_/  /--'_.' ,'
# (il),-''  (li),'  ((!.-'  



#################################################################################
# Merge everything into 1 dataframe
#################################################################################

merge_bsc <- graduate_bsc6[,c(6,11:20,22:27,29,30)]

total_credits <- distinct(filter_graduate_1[,c('std_number', 'total_credit')])

head(programs_count)

head(programs)

head(mean_grade_per_program)

head(mean_grade_per_year1)

graduate_bsc5_first <- distinct(
  graduate_bsc5_first[,c('std_number', 'no_of_ABCDEF_grades','mean_first_half')])

graduate_bsc5_last <- distinct(graduate_bsc5_last[,c('std_number','mean_last_half')])

## Course grade columns with less than 30% NA
head(bscgrades_wide_min2)
head(bscgrades_wide_max2)

# MERGE
df_bsc_merged <- full_join(merge_bsc, total_credits, by = 'std_number')
df_bsc_merged <- full_join(df_bsc_merged, programs_count, by = 'std_number')
df_bsc_merged <- full_join(df_bsc_merged, programs, by = 'std_number')
df_bsc_merged <- full_join(df_bsc_merged, mean_grade_per_program, by = 'std_number')
df_bsc_merged <- full_join(df_bsc_merged, mean_grade_per_year1, by = 'std_number')
df_bsc_merged <- full_join(df_bsc_merged, graduate_bsc5_first, by = 'std_number')
df_bsc_merged <- full_join(df_bsc_merged, graduate_bsc5_last, by = 'std_number')

df_bsc_merged$program.y <- ifelse(df_bsc_merged$program.y == '2504, 2501', '2501, 2504', 
                                  df_bsc_merged$program.y) # change '2504, 2501' to '2501, 2504'
df_bsc_merged$program.y <- as.factor(df_bsc_merged$program.y)
levels(df_bsc_merged$program.y)

df_bsc_merged <- distinct(df_bsc_merged) # remove duplicate rows

# check
nrow(df_bsc_merged)


#################################################################################
# CREATE VARIABLE FOR DIFFERENCE BETWEEN FIRST HALF AND SECOND HALF
#################################################################################

df_bsc_merged <- df_bsc_merged %>%
  mutate(diff_last_first = mean_last_half - mean_first_half)


#################################################################################
#################################################################################
#################################################################################

# After this, we also tried to construct some reliable variables from the ranking
# and from the leads dataset.


#################################################################################
# MERGE BROCHURE DATA
#################################################################################

names(brochure)
head(brochure)

# Drop columns: "Student ID (Pseudo)", "Year of Birth", "Nationality Group (Bin)" 
brochure[,c(4,6,7)] <- NULL
names(brochure) <- c('brochure_type', 'brochure_id', 'year', 'std_number', 'expected_entry')

# inspecting expected entry column: values dont make sense --> drop
brochure[,5] <- NULL
head(brochure)
nrow(brochure)

# drop rows with missing std_number --> internal students have student number
brochure1 <- brochure %>%
  filter(!is.na(std_number)) 
nrow(brochure1)

# create columns with number of brochures per program
brochure_program <- dcast(brochure1, std_number ~ brochure_id, value.var = 'brochure_id', 
                          fun.aggregate=function(x) length(x))
brochure_program_yesno <- dcast(brochure1, std_number ~ brochure_id, value.var = 'brochure_id', 
                                fun.aggregate=function(x) length(unique(x)))
head(brochure_program_yesno)

# create columns with number of brochures bsc & msc
brochure_bsc_msc <- dcast(brochure1, std_number ~ brochure_type, value.var = 'brochure_type', 
                          fun.aggregate=function(x) length(x))
brochure_bsc_msc_yesno <- dcast(brochure1, std_number ~ brochure_type, value.var = 'brochure_type', 
                                fun.aggregate=function(x) length(unique(x)))
head(brochure_bsc_msc_yesno)


# Merge brochure columns to dataset
df_bsc_merged <- left_join(df_bsc_merged, brochure_bsc_msc_yesno, by = 'std_number')
df_bsc_merged <- left_join(df_bsc_merged, brochure_program_yesno, by = 'std_number')


#################################################################################
# Check missings
#################################################################################

missmap(df_bsc_merged, y.labels = NULL, y.at = NULL)

# Check students with both programs
df_bsc_merged %>%
  filter(program.y == '2501, 2504') # 29 students with 2 BSc programs


#################################################################################
# MERGE RANKING/BIN data
#################################################################################

head(ranking_EBE)
head(ranking_IB)
dim(ranking_EBE)
dim(ranking_IB)

# Combine both
msc_bsc_bin_outcome <- rbind(ranking_EBE, ranking_IB)

colnames(msc_bsc_bin_outcome)
names(msc_bsc_bin_outcome) <- c('bin', 'std_number', 'year')

nrow(msc_bsc_bin_outcome)
length(unique(msc_bsc_bin_outcome$std_number)) # some students with more bins

msc_bsc_bin_outcome <- msc_bsc_bin_outcome %>%
  group_by(std_number) %>%
  mutate(n_bins = ifelse(is.na(bin), NA, n())) %>%
  mutate(max_bin = min(bin)) %>% # min function for max bin (lower is better)
  mutate(min_bin = max(bin)) 

msc_bsc_bin_outcome$bin <- NULL # drop bin column
msc_bsc_bin_outcome$year <- NULL # drop year column

msc_bsc_bin_outcome <- distinct(msc_bsc_bin_outcome) # drop duplicate rows

msc_bsc_bin_outcome %>%
  group_by(std_number) %>%
  filter(n() >1) 
# no more duplicates

# Ready for merge
df_bsc_merged <- left_join(df_bsc_merged, msc_bsc_bin_outcome, by = 'std_number')

nrow(df_bsc_merged)


#################################################################################
# Merge resits information to dataframe
#################################################################################

head(max_resits)
head(total_resits)

df_bsc_merged <- left_join(df_bsc_merged, max_resits, by = 'std_number')
df_bsc_merged <- left_join(df_bsc_merged, total_resits, by = 'std_number')

nrow(df_bsc_merged)


#################################################################################
# ADD PRE-EDUCATION
#################################################################################


head(pre_educ)

names(pre_educ) <- c('academic_start', 'pre_education', 'pre_education_country', 
                     'no_total_admissions', 'std_number', 'academic_year_pre_educ', 'program')

head(pre_educ)

# Filter out pre-education for the MSc
pre_educ_bsc <- pre_educ %>%
  group_by(std_number) %>%
  filter(!(program == 2604)) # filter out pre-educ MSc

# Create dummy variables for each category of pre-education, since some
# students have multiple pre-education rows, it made most sense to create dummies
# instead of 1 variable with all possible combinations.

# Create dummies
pre_educ_bsc <- pre_educ_bsc %>%
  group_by(std_number) %>%
  mutate(pre_educ_Foreign = ifelse(any(pre_education == 'Foreign'), 1,0))

pre_educ_bsc <- pre_educ_bsc %>%
  mutate(applied_both_bsc_programs = ifelse(
    any(program == 2501) & any(program == 2504), 1, 0)) %>%
  mutate(pre_educ_VWO = ifelse(any(pre_education == 'VWO'), 1, 0)) %>%
  mutate(pre_educ_HBO = ifelse(any(pre_education == 'HBO'), 1, 0)) %>%
  mutate(pre_educ_Uni_Bachelor = ifelse(any(pre_education == 'Uni. Bachelor'), 1, 0)) %>%
  mutate(pre_educ_Uni_Master = ifelse(any(pre_education == 'Uni. Master'), 1, 0)) %>%
  mutate(pre_educ_Other = ifelse(any(pre_education == 'Other'), 1, 0))

foreign_students <- pre_educ_bsc %>%
  group_by(std_number) %>%
  select(pre_education, pre_education_country, std_number, pre_educ_Foreign, 
         applied_both_bsc_programs) %>%
  distinct() %>%
  filter(!(any(pre_education_country %in% c('', 'The Netherlands')))) %>%
  mutate(pre_educ_country = pre_education_country)

pre_educ_bsc <- full_join(pre_educ_bsc, foreign_students, by = 'std_number')

pre_educ_bsc %>%
  group_by(std_number) %>%
  filter(n() > 1) %>%
  View()

pre_educ_bsc <- pre_educ_bsc %>%
  group_by(std_number) %>%
  select(std_number, pre_education.x, pre_education_country.x, applied_both_bsc_programs.x, 
         pre_educ_Foreign.x, pre_educ_HBO, pre_educ_VWO, pre_educ_Uni_Bachelor, pre_educ_Uni_Master,
         pre_educ_Other, pre_educ_country) %>%
  mutate(pre_educ_country = ifelse(is.na(pre_educ_country), 
                                   ifelse(pre_educ_Foreign.x == 1, 'Unknown', 'The Netherlands'), 
                                   as.character(pre_educ_country))) %>%
  select(-pre_education.x, -pre_education_country.x) %>%
  distinct() %>%
  # keep only last row of group (to remove education country is unknown when duplicates)
  top_n(-1, wt = pre_educ_country) 

head(pre_educ_bsc)

pre_educ_bsc %>%
  filter(n()>1) %>%
  View()

# Merge to df
df_bsc_merged<- left_join(df_bsc_merged, pre_educ_bsc, by = 'std_number')

View(df_bsc_merged)


# reading code line by line can be quite burdensome. A cat every now and then is well deserved. This one is Christoph.  
#
#                     _
#                    \`\
#         /./././.   | |
#       /        `/. | |
#     /     __    `/'/'
#  /\__/\ /'  `\    /
# |  oo  |      `.,.|
#  \vvvv/        ||||
#    ||||        ||||
#    ||||        ||||
#    `'`'        `'`'


#################################################################################
# MERGE MASTER DATA (DEMOGRAPHICS AND OUTCOME VARIABLE)
#################################################################################

# load 'Master' sheet of the Bachelors datast
names(bachelor_merge)
names(bachelor_merge) <- c('gender_bsc', 'std_number', 'year_of_birth_bsc', 'nationality_bsc')
head(bachelor_merge)
nrow(bachelor_merge)

# Add additional BSc 2012 data
names(bachelor_merge_2012)
names(bachelor_merge_2012) <- c('gender_bsc', 'std_number', 'year_of_birth_bsc', 'nationality_bsc')
head(bachelor_merge_2012)
nrow(bachelor_merge_2012)

# Combine 2012 data with other BSc data
bachelor_merge <- rbind(bachelor_merge, bachelor_merge_2012)

# remove duplicates
bachelor_merge <- bachelor_merge %>%
  distinct()
nrow(bachelor_merge)


# load master sheet of MSc file to merge with merged file
names(master_merge) <- c('gender_msc', 'std_number', 'year_of_birth_msc', 'nationality_msc')
head(master_merge)
nrow(master_merge)

# add additional 2018 data
names(master_merge_2018) <- c('gender_msc', 'std_number', 'year_of_birth_msc', 'nationality_msc')
head(master_merge_2018)
nrow(master_merge_2018)

# Combine 2018 data with other MSc data
master_merge <- rbind(master_merge, master_merge_2018)

# Remove duplicates
master_merge <- master_merge %>%
  distinct()

nrow(master_merge)

# Students that appear in both files
msc_bsc_ranking_merge <- left_join(bachelor_merge, master_merge, by = 'std_number')

# check data cleanliness
bsc_msc <- !is.na(msc_bsc_ranking_merge$nationality_msc) 

# dataframe with students who are in bachelor & master file
bsc_msc_UM <- msc_bsc_ranking_merge[c(bsc_msc),] 
summary(bsc_msc_UM)
nrow(bsc_msc_UM) # 2012 students who are in both bsc & msc master file

# unit tests for checking if there are no different genders, year of birth or nationality in the
# bsc master file and msc master file
all(bsc_msc_UM$nationality_msc == bsc_msc_UM$nationality_bsc)
all(bsc_msc_UM$gender_msc == bsc_msc_UM$gender_bsc)
all(bsc_msc_UM$year_of_birth_msc == bsc_msc_UM$year_of_birth_bsc)
# data is clean for merged students
# we can drop nationality_msc/gender_msc/year_of_birth_msc

# Create output column in_master_um_file: yes(1), no (0)
msc_bsc_ranking_merge$in_master_um_file <- ifelse(!is.na(msc_bsc_ranking_merge$gender_msc), 1,0)

colnames(msc_bsc_ranking_merge)

# Drop nationality_msc/gender_msc/year_of_birth_msc
msc_bsc_ranking_merge[,c(5,6,7)] <- NULL
head(msc_bsc_ranking_merge)

# drop duplicates
msc_bsc_ranking_merge <- msc_bsc_ranking_merge %>%
  distinct()


# Merge to final df
df_bsc_merged <- left_join(df_bsc_merged, msc_bsc_ranking_merge, by = 'std_number')

#################################################################################
# MISSMAP
#################################################################################

missmap(df_bsc_merged, y.labels = NULL, y.at = NULL)

View(df_bsc_merged)
nrow(df_bsc_merged)

df_bsc_merged %>%
  distinct() %>%
  nrow()

length(unique(df_bsc_merged$std_number))

df_bsc_merged %>%
  group_by(std_number) %>%
  filter(n() > 1) %>%
  View()

df_bsc_merged %>%
  filter(in_master_um_file == 1) %>%
  nrow() 

# 768 of the 1882 students also appear in the master file


#################################################################################
# To CSV
#################################################################################

View(df_bsc_merged)

write.csv(df_bsc_merged, file = 'grades_demographics_ranking_brochure_program.csv')
write.csv(bscgrades_wide_count, file = 'bscgrades_count_per_course.csv')
write.csv(bscgrades_wide_max, file = 'bscgrades_max_per_course.csv')
write.csv(bscgrades_wide_min, file = 'bscgrades_min_per_course.csv')


######################################################################################################################
#          Pierre Hardy's Part        
######################################################################################################################

### Some binding between the original dataset and the extra dataset

# combine extra bachelor admissions data
admissions <- bind_rows(admissions, extra.admissions) 
admissions <- arrange(admissions, admissions$`Student Number (Pseudo)`)

# combine extra bachelor registrations data
registrations <- bind_rows(registrations, extra.registrations)
registrations <- arrange(registrations, registrations$`Student Number (Pseudo)`)

# combine extra master registrations data
masters <- bind_rows(masters, extra.masters)
masters <- arrange(masters, masters$`Student Number (Pseudo)`)

length.unique <- length(unique(admissions$`Student Number (Pseudo)`))

### combining with another groupmate's output
roepie <- df_bsc_merged

### Creates main data frame ##########################################################################################

# one row per student number
main.data.clean <- unique(admissions$`Student Number (Pseudo)`) 
# declare that is it a data frame
main.data.clean <- as.data.frame(main.data.clean) 
# rename the column 
colnames(main.data.clean)[colnames(main.data.clean)=="main.data.clean"] <- "Student ID" 


### How many times each student applied? #############################################################################

# We get this information in the bachelor dataset within the admission sheet. We don't include re-registrations and 
# registrations for an addiotnal program. We only want those who apply to start their study in SBE
rows.sans.rereg <- filter(admissions, admissions$`Admission Category (Description)` != "Re-Registration")
rows.sans.rereg <- filter(rows.sans.rereg, rows.sans.rereg$`Prog. Choice (Desc.)` != "Additional program")
# create two columns: one of the student numbers and one of the number of times a student number appeared in the 
# admissions dataset (this serves as the number of times a student applied in SBE). Only one row per student. 
no.app <- matrix(ncol=2, nrow=length(unique(rows.sans.rereg$`Student Number (Pseudo)`)))
# rename the columns 
colnames(no.app) <- c("Student ID", "Number Applications")
# count how much unique students there are
no.app[,1] <- unique(rows.sans.rereg$`Student Number (Pseudo)`)
# count how often this student number appeared in the sheet
no.app[,2] <- table(rows.sans.rereg$`Student Number (Pseudo)`)
# merge the resulting tally to the main data frame. Merging by student ID
main.data.clean <- merge(main.data.clean, no.app, by="Student ID", all=TRUE)


### How many were accepted by SBE? ###################################################################################

# we re-use the data frame that has already filtered out re-registers and additional program 
no.accept <- rows.sans.rereg
# create an empty column 
no.accept$`Admission Appl. Status (Desc.)` <- 0
# assign one to all the students that were approved 
no.accept$`Admission Appl. Status (Desc.)`[rows.sans.rereg$`Admission Appl. Status (Desc.)`=="Approved"] <- 1
# only take the columns of interest (student number and those accepted)
no.accept <- select(no.accept, c("Admission Appl. Status (Desc.)","Student Number (Pseudo)"))
# this takes only the maximum value in a way that if there's a 1, it'll only take one. This is done because there are
# students with multiple applications and were accepted to only one. 
no.accept <- aggregate(no.accept$`Admission Appl. Status (Desc.)`, by=list(no.accept$`Student Number (Pseudo)`), 
                       FUN=max)
# rename the columns
colnames(no.accept) <- c("Student ID","Approved")
# merge it to the main data frame  
main.data.clean <- merge(main.data.clean, no.accept, by="Student ID", all=TRUE)


### How many years registered in SBE? #################################################################################

#  filter out the unique student number in the registrations sheet of the bachelor dataset 
no.yrs.reg <- distinct(select(registrations, c("Student Number (Pseudo)")))
# count how many times a student number appeared in the sheet 
# (which is how many years a student was registered in SBE)
no.yrs.reg <- mutate(no.yrs.reg, table(registrations$`Student Number (Pseudo)`))
# rename the columns
colnames(no.yrs.reg) <- c("Student ID","Years Registered")
# merge it to the main dataframe by student ID
main.data.clean <- merge(main.data.clean, no.yrs.reg, by="Student ID", all=TRUE)


### at which year did the student first start in SBE? ################################################################

# we re-use the column that contains all the student in the admissions sheet that are not re-reg or add. progs. and 
# filter only those that were accepted. This is to ensure that we get only those that actually start their very first 
# year
start.yr <- filter(no.accept, no.accept$Approved==1) 
# rename the columns
colnames(start.yr) <- c("Student Number (Pseudo)","Approved")
# join the registrations sheet and the starting year. This ensure the only the student number that appeared in the 
# registrations (that are not re-reg and add. prog.) and admissions sheet is included. 
start.yr <- inner_join(registrations, start.yr, by="Student Number (Pseudo)")
# select only the column we need (student number and years they are registered)
start.yr <- select(start.yr, c("Student Number (Pseudo)", "Academic Year (Pseudo)"))
# keep only the earliest year (assuming this is their very first year in SBE)
start.yr <- aggregate(start.yr$`Academic Year (Pseudo)`, by=list(start.yr$`Student Number (Pseudo)`), FUN=min)
# rename the columns 
colnames(start.yr) <- c("Student ID","Starting Year")
# merge
main.data.clean <- merge(main.data.clean, start.yr, by="Student ID", all=TRUE)


# reading code line by line can be quite burdensome. A cat every now and then is well deserved. This one is Sebastian.
#  
#            _,'|             _.-''``-...___..--';)
#          /_ \'.      __..-' ,      ,--...--'''
#        <\    .`--'''       `     /'
#         `-';'               ;   ; ;
#   __...--''     ___...--_..'  .;.'
# (,__....----'''       (,..--''


### Did the student graduate? ########################################################################################

# we use the dataset that already shows which student number graduated  
grad <- graduates
# rename the columns 
colnames(grad) <- c("Program Graduated","Graduated", "Student ID")
# puts 1 if the student gradauted
grad$Graduated <- grad$Graduated*1
# filter out only those who graduated
grad <- filter(grad, grad$Graduated==1)
# merge
main.data.clean <- merge(main.data.clean, grad, by="Student ID", all=TRUE)
# put 0 to everyone else that wasnt in the graduation dataset
main.data.clean[["Graduated"]][is.na(main.data.clean[["Graduated"]])] <- 0


### What is the graduate's graduation year? ##########################################################################

# filter out the students that graduates  
grad.yr <- filter(graduates, graduates$`Graduated?`==TRUE)
# create a data frame that include student numbers that are both in the registrations sheet and the graduates
grad.yr2 <- inner_join(registrations, grad.yr, by="Student Number (Pseudo)")
# select only the student number and years registered 
grad.yr2 <- select(grad.yr2, c("Student Number (Pseudo)","Academic Year (Pseudo)"))
# get the latest year they registered in UM (we assume this is their final and graduating year)
grad.yr2 <- aggregate(grad.yr2$`Academic Year (Pseudo)`, by=list(grad.yr2$`Student Number (Pseudo)`), FUN=max)
# we assume that they register in September and graduate november of the following year that's why we add plus one 
# year
grad.yr2$x <- grad.yr2$x+1
# rename
colnames(grad.yr2) <- c("Student ID", "Graduation Year")
# merge
main.data.clean <- merge(main.data.clean, grad.yr2, by="Student ID", all=TRUE)


### Target Variable 1: student did their bachelor here and also their masters ########################################

# "masters" contain the registrations sheet of the original and extra dataset and "admissions.master" contains the 
# admissions sheet. "Master.both" includes both of those who applied and actually registered. 
master.both <- bind_rows(masters, admissions.master)
# create a data frame of student numbers that appeared both in the bachelor registrations sheet and master datasets.
upsold <- inner_join(registrations, master.both, by="Student Number (Pseudo)")
upsold <- select(upsold, c("Student Number (Pseudo)"))
upsold <- distinct(upsold)
# create a column that assigns one to the all students in the upsold data frame and rename the columns 
upsold <- mutate(upsold, Bachelor.2.Master = 1)
colnames(upsold) <- c("Student ID", "Bachelor and Master Present")
# merge 
main.data.clean <- merge(main.data.clean, upsold, by="Student ID", all=TRUE)


### How many times did a student apply for a master? #################################################################

# create an admissions sheet for masters minus the re-regs and add. progs.   
rows.sans.rereg.master <- filter(admissions.master, 
                                 admissions.master$`Admission Category (Description)` != "Re-Registration")
rows.sans.rereg.master <- filter(rows.sans.rereg.master, 
                                 rows.sans.rereg.master$`Prog. Choice (Desc.)` != "Additional program")
# select only the student number 
rows.sans.rereg.master2 <- select(rows.sans.rereg.master, c("Student Number (Pseudo)"))
#   create another data frame with only one row per student number
rows.sans.rereg.master <- distinct(select(rows.sans.rereg.master, c("Student Number (Pseudo)")))
# mutate the two so that there are only one row per student number but the table function also counts how many times
# the student number appeared in the admissions sheet (which we assume is the amount of times a student applied for 
# the master)
rows.sans.rereg.master <- mutate(rows.sans.rereg.master, table(rows.sans.rereg.master2$`Student Number (Pseudo)`))
# rename and merge
colnames(rows.sans.rereg.master) <- c("Student ID", "Number Applications Master")
main.data.clean <- merge(main.data.clean, rows.sans.rereg.master, by="Student ID", all=TRUE)



### Is the student accepted for a master study in SBE? ###############################################################

# we recreate the master admissions dataset minus the re-reg and add. progs. 
no.accept.master <- filter(admissions.master, 
                           admissions.master$`Admission Category (Description)` != "Re-Registration")
no.accept.master <- filter(no.accept.master, no.accept.master$`Prog. Choice (Desc.)` != "Additional program")
no.accept.master2 <- no.accept.master
# we put 1 to those who got accepted in their application (has "Approvde" in the admission appl stats) and 0 to not
no.accept.master$`Admission Appl. Status (Desc.)` <- 0
no.accept.master$`Admission Appl. Status (Desc.)`[no.accept.master2$`Admission Appl. Status (Desc.)`=="Approved"] <- 1
# select the appropriate columns, make it only one student per row, rename columns, then merge. Similar to the past 
# procedures. 
no.accept.master <- select(no.accept.master, c("Student Number (Pseudo)","Admission Appl. Status (Desc.)"))
no.accept.master <- aggregate(no.accept.master$`Admission Appl. Status (Desc.)`,
                              by=list(no.accept.master$`Student Number (Pseudo)`), FUN=max)
colnames(no.accept.master) <- c("Student ID","Master Approved")
main.data.clean <- merge(main.data.clean, no.accept.master, by="Student ID", all=TRUE)


### Years registered in Masters? #####################################################################################

# the process is similar to the procedure of getting years registered in bachelor. Kindly refer to that part of the 
# code a bit above for a more in-depth explanation. 

no.yrs.reg.master <- distinct(select(masters, c("Student Number (Pseudo)")))
no.yrs.reg.master <- mutate(no.yrs.reg.master, table(masters$`Student Number (Pseudo)`))
colnames(no.yrs.reg.master) <- c("Student ID","Years Registered Master")
main.data.clean <- merge(main.data.clean, no.yrs.reg.master, by="Student ID", all=TRUE)

### Start Year Masters? ##############################################################################################

# the process is similar to the procedure of getting starting year in bachelor. Kindly refer to that part of the code
# a bit above for a more in-depth explanation.   
start.yr.master <- filter(no.accept.master, no.accept.master$`Master Approved`==1)
colnames(start.yr.master) <- c("Student Number (Pseudo)"," Master Approved")
start.yr.master <- inner_join(masters, start.yr.master, by="Student Number (Pseudo)")
start.yr.master <- select(start.yr.master, c("Student Number (Pseudo)", "Academic Year (Pseudo)"))
start.yr.master <- aggregate(start.yr.master$`Academic Year (Pseudo)`, 
                             by=list(start.yr.master$`Student Number (Pseudo)`), FUN=min)
colnames(start.yr.master) <- c("Student ID","Starting Year Master")
main.data.clean <- merge(main.data.clean, start.yr.master, by="Student ID", all=TRUE)


### target attribute finishing touch #################################################################################

# we just fix our target variable 1 by assigning 0 instead of NA to those that didn't appear in both the master and
# bachelor dataset
main.data.clean[["Bachelor and Master Present"]][is.na(main.data.clean[["Bachelor and Master Present"]])] <- 0


### how many attended events? ########################################################################################

# select columns with the student number and the events their attended  
events <- select(events, c("Campaign (Pseudo)", "Student Number (Pseudo)"))
# create two columns that does not differentiate between open days of different years becase it will spread the counts 
# too thin and will not give much informational value if ever. It will only differentiate if it was a bachelor or 
# master open day. 
events$Bachelor.Open.Day <-  ifelse(substr(events$`Campaign (Pseudo)`, start=1, stop=4) == "Bach", 1, 0)
events$Master.Open.Day <-  ifelse(substr(events$`Campaign (Pseudo)`, start=1, stop=4) == "Mast", 1, 0)
# select the columns, rename columns, merge. Akin to the processes above. 
events <- select(events, c("Student Number (Pseudo)", "Bachelor.Open.Day", "Master.Open.Day"))
colnames(events)[1] <- "Student ID"
main.data.clean <- merge(main.data.clean, events, by="Student ID", all=TRUE)
# clean the data so that instead of NA, it says 0 
main.data.clean[["Bachelor.Open.Day"]][is.na(main.data.clean[["Bachelor.Open.Day"]])] <- 0
main.data.clean[["Master.Open.Day"]][is.na(main.data.clean[["Master.Open.Day"]])] <- 0


### what is the bachelor specailization? #############################################################################

# we use the extra data telling the specialization of a bachelor student
colnames(special) <- c("Program", "Specialization", "Student ID")
# we remove the program 
special <- special[-1]
# merge with the main data frame
main.data.clean <- merge(main.data.clean, special, by="Student ID", all=TRUE)


### we include the master grades #####################################################################################

# this code is an excerpt of another group member's code. It has been repurposed to accomodate the master's grade 
master.grades <- select(master.grades, c("Student Number (Pseudo)","Appraisal Type","Module (Abbrev.) (Pseudo)",
                                         "Academic Year (Pseudo)","Academic Session","Appraisal date (Pseudo)",
                                         "Grade (Bin)", "Program (Abbreviation)"))
master.grades2 <- master.grades
colnames(master.grades2) <- c("std_number", "appraisal_type", "course_code", "year", "period", "date", "grade", 
                              "program")
graduate_msc_count <- master.grades2 %>%
  group_by(std_number) %>%
  filter(appraisal_type == 10 | appraisal_type == 7055) %>%
  group_by(std_number, course_code) %>%
  arrange(std_number, year, period, course_code, desc(appraisal_type), date) %>% # descending because of distinct 
  #  function later (keeps only first row)
  filter(!(any(grade %in% c('A', 'B', 'C')) & grade == 'PASS')) %>% # remove all PASS for courses for which there is 
  #  also a grade (A-C)
  filter(!(any(grade %in% c('D', 'E', 'F')) & grade == 'FAIL')) %>% # remove all FAILS for courses for which there is 
  #  also a grade (D-F)
  filter(!(any(grade %in% c('A', 'B', 'C', 'D', 'E', 'F', 'PASS', 'FAIL', 'EXCELLENT')) 
           & grade == 'NO GRADE')) %>% # remove all NO GRADE for which there is also a grade
  distinct(program, period, std_number, course_code, year, grade, date, .keep_all = TRUE) %>% # remove all duplicates 
  #  in every column except for appraisal type and credits (we can remove grade to allow for different grades)
  #distinct() %>% does not remove any extra rows --> no duplicates
  #filter(n()>1) %>% # show courses with more than one grade
  #filter((any(appraisal_type == 7055) & appraisal_type == 10)| 
  #(any(appraisal_type == 10) & appraisal_type == 7055)) %>% # manually check courses for which we have both appraisal 
  #  type 10
  filter(!(any(appraisal_type == 7055) & appraisal_type == 10)) # filter our 170 cases of courses for which we have a 
# grade in appraisal type 10 that is different from one of the grades in appraisal type 7055 appraisal type 10
graduate_msc_count <- ungroup(graduate_msc_count)
graduate_msc_count <- filter(graduate_msc_count, is.na(graduate_msc_count$grade)==FALSE)
# Replace grades with numbers
# replace_grade <- c(9, 7.5, 6, 5, 4, 3) # scale OK? 
# grade <- c('A', 'B', 'C', 'D', 'E', 'F')
grade.values <- data.frame(name=c("A","B","C","D","E","F","PASS", "NO GRADE","FAIL"))
grade.values$group <- as.numeric(c(9, 7.5, 6, 5, 4, 3, 7.5, 0,0))
graduate_msc_count$numeric.grade <- match(graduate_msc_count$grade, grade.values$name)
graduate_msc_count$numeric.grade <- grade.values$group[graduate_msc_count$numeric.grade]
master.grades3 <- graduate_msc_count %>%
  group_by(std_number) %>%
  summarise(mean_grade=mean(numeric.grade))
# include in the dataset (rename columns then merge)
colnames(master.grades3)[1] <- "Student ID"
colnames(master.grades3)[2] <- "Mean Grade Master"
main.data.clean <- merge(main.data.clean, master.grades3, by="Student ID", all=TRUE)


### merge Roepie's output with Pierre's ##############################################################################
colnames(roepie)[1] <- "Student ID"
main.data.clean <- merge(main.data.clean, roepie, by="Student ID", all=TRUE)

### view the main clean data set, primed and ready for modeling ######################################################
# left as a comment unless wanted
# View(main.data.clean)


### filter out useless datarows. #####################################################################################
# The cleaning was made to be as inclusive as possible at it is good practice to include more than what is immedaitely
# required because it is easier to exclude rows and columns that to re-code the cleaning to include a part that was
# relized halfway to be needed. However this part was included to ease the transition to the modelling phase of the 
# project. This can, and has been, adjusted as the proejct went along. For now, needs are met as it stands.
main.data.cleaner <- filter(main.data.clean, is.na(main.data.clean$mean_grade)==FALSE)

### new target variable 3
# 1 - did bachelor but not master
# 2 - did bachelor, applied master, but did not continue (to be exact, no master grade but apperaed in the file)
# 3 - did bachelor, became a mediocre master student
# 4 - did bachelor, became a great master student like the members of this project
# threshhold - 7.5
# UPDATE: it has been decided to remove 2 and turn 3 into 2 and 4 into 3. The reason is that it contains people who 
# applied but withdrew their application (130 samples) and those who started their master but their grades weren't
# in the dataset yet (168 samples). We decided to leave them out entirely because of their small size, they are too
# much work for little to no information gain
# Therefore: 
# 1 - did bachelor but not master 
# 2 - did bachelor, became a mediocre master student
# 3 - did bachelor, became a great master student like the members of this project
# Threshold still 7.5
colnames(main.data.cleaner)[9] <- "Bachelor_and_Master_Present" 
colnames(main.data.cleaner)[17] <- "Mean_Grade_Master"
main.data.cleaner$new.target.var <- NA
main.data.cleaner <- main.data.cleaner %>%
  mutate(new.target.var = ifelse(Bachelor_and_Master_Present==0,1,
                                 ifelse(Bachelor_and_Master_Present==1 & Mean_Grade_Master<7.5,2,
                                        ifelse(Bachelor_and_Master_Present==1 & Mean_Grade_Master>=7.5,3,0))))


### export
write.csv(main.data.cleaner, file="Group 8 - Cleaned Dataset.csv", row.names = FALSE)

# \\\\\_____________ UNTO MODELLING _____________\"-._
# |||||~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|.-'

