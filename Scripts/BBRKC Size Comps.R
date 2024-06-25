#Goal: Outputing 1mm binned data for BBRKC Females >90mm CL
  #Generated for K. Palof for 2024 BBRKC resampling decision:
#1) 1mm binned counts for all BBRKC females >90mm CL
#2) 1mmm binned counts for BBRKC females >90 mm CL, MATURE ONLY

#NOTE: Using most recent FTP'd data only- Products are preliminary until QA/QC! 

#Author: E. Fedewa

#load
library(tidyverse)

################

#Set path for most recent FTP'd data on Kodiak server 
path <- "Y:/KOD_Survey/EBS Shelf/2024/RawData/"

#Now read in all specimen tables from FTP'd data 
dat <- list.files(path, pattern = "CRAB_SPECIMEN", recursive = TRUE) %>% 
  purrr::map_df(~read.csv(paste0(path, .x)))

#Designate special project hauls to drop until haul types are assigned (see resampling script)
AKK_drop <- as.data.frame(c(1:17,23,25,45)) %>% setNames("HAUL")           
NWE_drop <- as.data.frame(c(1:16,21,23,28:29,37,39,60)) %>% setNames("HAUL")

#Create Look up table for Bristol Bay stations, including Z-04 
  #hard coding so this can be run on the boat w/out file dependency issues! 
BBRKC_DIST <- data.frame("A-02","A-03","A-04","A-05","A-06","B-01","B-02","B-03","B-04","B-05","B-06",
                         "B-07","B-08","C-01","C-02","C-03","C-04","C-05","C-06","C-07","C-08","C-09","D-01",
                         "D-02","D-03","D-04","D-05","D-06","D-07","D-08","D-09","D-10","E-01","E-02","E-03",
                         "E-04","E-05","E-06","E-07","E-08","E-09","E-10","E-11","E-12","F-01","F-02","F-03",
                         "F-04","F-05","F-06","F-07","F-08","F-09","F-10","F-11","F-12","F-13","F-14","G-01",
                         "G-02","G-03","G-04","G-05","G-06","G-07","G-08","G-09","G-10","G-11","G-12","G-13",
                         "G-14","G-15","H-01","H-02","H-03","H-04","H-05","H-06","H-07","H-08","H-09","H-10",
                         "H-11","H-12","H-13","H-14","H-15","H-16","I-01","I-02","I-03","I-04","I-05","I-06",
                         "I-07","I-08","I-09","I-10","I-11","I-12","I-13","I-14","I-15","I-16","J-01","J-02",
                         "J-03","J-04","J-05","J-06","J-07","J-08","J-09","J-10","J-11","J-12","J-13","J-14",
                         "J-15","J-16","K-01","K-02","K-03","K-04","K-05","K-06","K-07","K-08","K-09","K-10",
                         "K-11","K-12","K-13","K-14","Z-05","Z-04")

#Filter for stations in BBRKC Mgmt district- dropping non-standard stations
data <- dat %>%
  dplyr::filter(!(VESSEL == 162 & HAUL %in% AKK_drop$HAUL),
                !(VESSEL == 134 & HAUL %in% NWE_drop$HAUL)) %>%
  filter(str_detect(STATION, "-")) %>% #additional filter to remove any corner stations
  #Standardize station name notation to ensure there were no station name tablet entry errors  
  separate(STATION, sep = "-", into = c("col", "row", "tow")) %>%
  filter(is.na(tow)) %>% #this will drop any "-B" 15 min tow stations
  select(-c(tow)) %>%
  unite("STATION", col:row, sep = "-") %>%
  filter(STATION %in% BBRKC_DIST)

#Compute sum of all females >90mm CL by 1mm size bin   
data %>%
  filter(SPECIES_CODE == 69322,
         SEX == 2,
         LENGTH > 90) %>%
  #create 1mm size bin
  mutate(SIZE_BIN = floor(LENGTH)) %>% 
  group_by(SIZE_BIN) %>%
  summarise(NO_CRAB = sum(SAMPLING_FACTOR)) %>%
  write.csv(file="./Output/BBRKC_Size_Comps.csv", row.names=FALSE)

#Compute sum of mature females >90mm CL by 1mm size bin   
data %>%
  filter(SPECIES_CODE == 69322,
         SEX == 2,
         LENGTH > 90,
         CLUTCH_SIZE!=0) %>%
  #create 1mm size bin
  mutate(SIZE_BIN = floor(LENGTH)) %>% 
  group_by(SIZE_BIN) %>%
  summarise(NO_CRAB = sum(SAMPLING_FACTOR)) %>%
  write.csv(file="./Output/BBRKC_Size_Comps_Mature.csv", row.names=FALSE)
  


  