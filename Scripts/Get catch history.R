#Goal: Find historical crab catches from previous years
  #Instructions: Change station = argument to station of interest. 
  #Note that once in the NBS, change to survey = "NBS"  

library(GAPsurvey)

##For previous year and only 1 station for all crab species ---------------------
get_catch_haul_history(survey = "EBS", years = 2021, station = "I-13",
                       species_codes = c(69323, 69322, 68560, 68580, 68590))   

##For multiple years and only 1 station for all crab species ---------------------
get_catch_haul_history(survey = "EBS", years = c(2018, 2019, 2021), station = "I-13",
                       species_codes = c(69323, 69322, 68560, 68580, 68590))   
