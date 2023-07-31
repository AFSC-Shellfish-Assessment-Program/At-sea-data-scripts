#Goal: Find historical crab catches from previous years
#Instructions: Change station = argument to station of interest. 
#Note that once in the NBS, change to survey = "NBS"  

#Initial install only
#library(devtools)
#devtools::install_github("afsc-gap-products/GAPsurvey")

library(GAPsurvey)

##For previous year and only 1 station for all crab species ---------------------
get_catch_haul_history(survey = "EBS", years = 2022, station = "S-22",
                       species_codes = c(69323, 69322, 68560, 68580, 68590))   

##For multiple years and only 1 station for all crab species ---------------------
get_catch_haul_history(survey = "EBS", years = c(2017, 2018, 2019, 2021, 2022), station = "U-25",
                       species_codes = c(69323, 69322, 68560, 68580, 68590))


##For previous year and catch at nearby stations for snow crab ---------------------
get_catch_haul_history(survey = "EBS", years = 2022, station = "I-09",
                       species_codes = 68560, grid_buffer = 3)

