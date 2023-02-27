# notes ----
#Calculate running sum of mature females within BB
#Calculate running thresholds for resampling

# Erin Fedewa
# last updated: 2022/7/21

# load ----
library(tidyverse)
library(RODBC)

#Step 1: Download specimen data
#a) Download final access database for each vessel AFTER survey leg is completed OR
#M:\EBS Shelf\2022\RawData (I copied Access databases manually per leg/boat)
#b) Download most recent FTP'd access database from ONGOING leg for both vessels 
#https://sftp.afsc.noaa.gov/ThinClient/WTM/public/index.html#/login

#Step 2: Import specimen table only from each access database (TO DO: create loop for this!)
db1 <- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/AKKN_2022_Leg1.accdb"
con1 <- odbcConnectAccess2007(db1)
sqlTables(con1)$TABLE_NAME
AKK_L1 <- sqlFetch(con1, "tblCRAB_SPECIMEN")

db2 <- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/AKKN_2022_Leg2.accdb"
con2 <- odbcConnectAccess2007(db2)
AKK_L2 <- sqlFetch(con2, "tblCRAB_SPECIMEN")

db3 <- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/VEST_2022_Leg1.accdb"
con3 <- odbcConnectAccess2007(db3)
VEST_L1 <- sqlFetch(con3, "tblCRAB_SPECIMEN")

db4<- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/VEST_2022_Leg2.accdb"
con4 <- odbcConnectAccess2007(db4)
VEST_L2 <- sqlFetch(con4, "tblCRAB_SPECIMEN")

#Step 3: Calculate running threshold
bind_rows(AKK_L1, AKK_L2, VEST_L1, VEST_L2) %>%
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
         !(VESSEL == 162 & HAUL %in% c(22, 25,37,39)), #Pulling out 2022 15 min tows 
         !(VESSEL == 94 & HAUL %in% c(26, 27, 41, 43)),
         STATION %in% BBRKC_DIST,
         SEX == 2,
         CLUTCH_SIZE!=0) %>% 
  summarize(Mat_fem_tot = sum(SAMPLING_FACTOR))

#Sum of females counting towards threshold 
data %>%
  filter(SPECIES_CODE == 69322,
         !(VESSEL == 162 & HAUL %in% c(22, 25,37,39)), #Pulling out 2022 15 min tows 
         !(VESSEL == 94 & HAUL %in% c(26, 27, 41, 43)),
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
         !(VESSEL == 162 & HAUL %in% c(22, 25,37,39)), #Pulling out 2022 15 min tows 
         !(VESSEL == 94 & HAUL %in% c(26, 27, 41, 43)),
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
  
  




























