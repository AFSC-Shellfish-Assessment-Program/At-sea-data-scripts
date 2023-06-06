# notes ----
#Calculate running sum of mature females within BB
#Calculate running thresholds for resampling

# Erin Fedewa
# last updated: 2022/7/21

# load ----
library(tidyverse)
library(RODBC)
library(data.table)
library(dplyr)
#Step 1: Download specimen data
#a) Download final access database for each vessel AFTER survey leg is completed OR
#M:\EBS Shelf\2022\RawData (I copied Access databases manually per leg/boat)
#b) Download most recent FTP'd access database from ONGOING leg for both vessels 
#https://sftp.afsc.noaa.gov/ThinClient/WTM/public/index.html#/login

### Create data objects

dat<-data.frame()  #empty data frame for adding data to

AKKN_drop_hauls<-c(22, 25,37,39) # Alaska Knight hauls that need to be excluded
VEST_drop_hauls<-c(26, 27, 41, 43) # Vesteraalen hauls that need to be excluded
NEWB_drop_hauls<-c()               # New boat hauls that need to be excluded

spec<- list.files("./Data/DataFromBoats/",pattern = "\\.accdb$")    #Creates list of database objects to be fed into for-loop
spec # it reads these in!

for(i in 1:length(spec)){ # start i loop (each file)
  #i <- 1:length(spec) #full data set
  path <- paste0("./Data/DataFromBoats/", spec[i], sep="")
  x<-odbcConnectAccess2007(path)
  out<-sqlFetch(x, "tblCRAB_SPECIMEN")
  out<-data.frame(out)
  dat<-rbind(dat,out)
  }



#Step 2: Calculate running threshold
dat %>%
  #Remove corner and test stations 
  filter(str_detect(STATION, "-")) %>%
  #Standardize station name notation 
  separate(STATION, sep = "-", into = c("col", "row")) %>%
  mutate(row = sprintf("%02d", as.numeric(row))) %>%
  unite("STATION", col:row, sep = "-") -> data 

#Create Look up table for Bristol Bay stations
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
                         "K-11","K-12","K-13","K-14","Z-05")

#Number of Mature Females  
data %>%
  filter(SPECIES_CODE == 69322,
         !(VESSEL == 162 & HAUL %in% AKKN_drop_hauls), #Pulling out 2022 15 min tows 
         !(VESSEL == 94 & HAUL %in% VEST_drop_hauls),
         STATION %in% BBRKC_DIST,
         SEX == 2,
         CLUTCH_SIZE!=0) %>% 
  summarize(Mat_fem_tot = sum(SAMPLING_FACTOR))

#Sum of females counting towards threshold 
data %>%
  filter(SPECIES_CODE == 69322,
         !(VESSEL == 162 & HAUL %in% AKKN_drop_hauls), #Pulling out 2022 15 min tows 
         !(VESSEL == 94 & HAUL %in% VEST_drop_hauls),
         STATION %in% BBRKC_DIST,
         SEX == 2,
         CLUTCH_SIZE!=0) %>% 
  mutate(Mat_fem_tot = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  mutate(clutch = ifelse(EGG_CONDITION==2, "Eyed",
                         ifelse(EGG_CONDITION==4 | EGG_CONDITION==0 & CLUTCH_SIZE==1, "Barren",
                                ifelse(EGG_CONDITION==5, "Hatching", NA)))) %>%
  filter(!is.na(clutch)) %>%
  summarise(clutch_sum = ((round(sum(SAMPLING_FACTOR,na.rm = T))))) 

#Threshold
data %>%
  filter(SPECIES_CODE == 69322,
         !(VESSEL == 162 & HAUL %in% AKKN_drop_hauls), #Pulling out 2022 15 min tows 
         !(VESSEL == 94 & HAUL %in% VEST_drop_hauls),
         STATION %in% BBRKC_DIST,
         SEX == 2,
         CLUTCH_SIZE!=0) %>% 
  mutate(Mat_fem_tot = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  mutate(clutch = ifelse(EGG_CONDITION==2, "Eyed",
                         ifelse(EGG_CONDITION==4 | EGG_CONDITION==0 & CLUTCH_SIZE==1, "Barren",
                                ifelse(EGG_CONDITION==5, "Hatching", NA)))) %>%
  filter(!is.na(clutch)) %>%
  group_by(clutch) %>%
  summarise(clutch_perc = ((round(sum(SAMPLING_FACTOR,na.rm = T)))/mean(Mat_fem_tot))*100,
            total_mature = mean(Mat_fem_tot)) %>%
  mutate(thres_total = sum(clutch_perc, na.rm = T))
  
  
