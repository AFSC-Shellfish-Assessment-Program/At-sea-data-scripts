# GOALS ----
#Calculate running sum of mature females within BB
#Calculate running threshold for resampling (> 10% triggers resampling)

#NOTE: This script should ONLY be used during the survey, as haul files have 
  #not yet been created to filter out special project tows by haul type

#Author: Erin Fedewa

# load ----
library(tidyverse)
library(RODBC)
library(gt)
library(webshot2)
library(magick)
library(gtExtras)

#########################

#Step 1: Set path for FTP'd data folder on Kodiak server
path <- "Y:/KOD_Survey/EBS Shelf/2024/RawData/"

#Step 2: Update non-standard stations sampled: (i.e. 15/30, Shelf/Slope special projects)
  #Add non-standard haul #'s from google sheet below to AKK_drop and NWX_drop objects 
#https://docs.google.com/spreadsheets/d/1yz9ANWaPO8634mDtfAJf8szXRdk0GyGwBDdS5cC418k/edit#gid=1916551151

AKK_drop <- c(18,20:23,26,42)                
NWX_drop <- c(1,3:6,22,25) 

#Step 3: Add zero crab catch stations (these are not included in the specimen table and we'll need them for a running station count)
#Add zero catch Station ID's from google sheet below to zero catch object  
#https://docs.google.com/spreadsheets/d/1yz9ANWaPO8634mDtfAJf8szXRdk0GyGwBDdS5cC418k/edit#gid=1916551151

zero_catch <- data.frame(c("K-14", "K-08", "A-06", "D-09", "K-07", "Z-05")) %>%
                  setNames("Station")

#Step 4: Update run date
  #This date object will be used to create separate output for each new resampling script run
  #Automatically updates based on the date run
run_date <- paste(format(Sys.time(),'%d'), format(Sys.time(),'%B'), format(Sys.time(),'%Y'), "run", sep = "_")


#####################

#Now read in all specimen tables from FTP'd data 
dat <- list.files(path, pattern = "CRAB_SPECIMEN", recursive = TRUE) %>% 
       purrr::map_df(~read.csv(paste0(path, .x)))

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
                         "K-11","K-12","K-13","K-14","Z-05", "Z-04")

#Filter for stations in BBRKC Mgmt district- dropping non-standard stations
data <- dat %>%
        filter(!(VESSEL == 162 & HAUL %in% AKK_drop),  
               !(VESSEL == 134 & HAUL %in% NWX_drop)) %>%
        filter(str_detect(STATION, "-")) %>% #additional filter to remove any corner stations
        #Standardize station name notation to ensure there were no station name tablet entry errors  
        separate(STATION, sep = "-", into = c("col", "row")) %>%
        mutate(row = sprintf("%02d", as.numeric(row))) %>% #this will drop any "-B" 15 min tow stations
        unite("STATION", col:row, sep = "-") %>%
        filter(STATION %in% BBRKC_DIST)

#Output csv for stations used in calculating resampling threshold
  #DOES NOT include zero catch stations
stations <- data %>%
            select(CRUISE, VESSEL, HAUL, STATION) %>%
            distinct(CRUISE, VESSEL, HAUL, STATION)
write.csv(stations, "./Output/Resample_station_list.csv")
#Please open this csv and visually double check that all non-standard stations were excluded!

#Number of Stations remaining in BBRKC mgmt district
station_count <- nrow(stations) #total # of positive catch stations sampled thus far
zero_count <- zero_catch %>% #total # of zero crab catch stations sampled thus far
              filter(Station %in% BBRKC_DIST) %>%
              nrow()
stations_remaining <- length(BBRKC_DIST) - (station_count + zero_count) 

#Number of Mature Females  
total_fem_sum <- data %>%
                 filter(SPECIES_CODE == 69322,
                        SEX == 2,
                        CLUTCH_SIZE!=0) %>% 
                 summarize(Mat_fem_tot = sum(SAMPLING_FACTOR))

#Sum of females counting towards threshold 
thresh_fem_sum <- data %>%
                  filter(SPECIES_CODE == 69322,
                         SEX == 2,
                         CLUTCH_SIZE!=0) %>% 
                  mutate(clutch = ifelse(EGG_CONDITION==2, "Eyed",
                                         ifelse(EGG_CONDITION==4 | EGG_CONDITION==0 & CLUTCH_SIZE==1, "Barren",
                                                ifelse(EGG_CONDITION==5, "Hatching", NA)))) %>%
                  filter(!is.na(clutch)) %>%
                  summarise(clutch_sum = ((round(sum(SAMPLING_FACTOR,na.rm = T)))))

#Threshold (Resampling triggered if > 10%)
(thresh_fem_sum/total_fem_sum) * 100

#Threshold broken down by clutch codes
clutch_thresh <- data %>%
                 filter(SPECIES_CODE == 69322,
                        SEX == 2,
                        CLUTCH_SIZE!=0) %>% 
                 mutate(Mat_fem_tot = sum(SAMPLING_FACTOR, na.rm = T)) %>%
                 mutate(clutch = ifelse(EGG_CONDITION==2, "Eyed",
                                       ifelse(EGG_CONDITION==4 | EGG_CONDITION==0 & CLUTCH_SIZE==1, "Barren",
                                               ifelse(EGG_CONDITION==5, "Hatching", NA)))) %>%
                 filter(!is.na(clutch)) %>%
                 group_by(clutch) %>%
                 summarise(thresh_fem_sum = ((round(sum(SAMPLING_FACTOR,na.rm = T)))),
                           clutch_perc = ((round(sum(SAMPLING_FACTOR,na.rm = T)))/mean(Mat_fem_tot))*100,
                           total_mature = mean(Mat_fem_tot)) %>%
                 mutate(thres_total = sum(clutch_perc, na.rm = T)) 

###########################

#Create summary tables and save as output by run date
  
#Table 1: total mature females/total unmolted/threshold
table1 <- data %>%
          filter(SPECIES_CODE == 69322,
                 SEX == 2,
                 CLUTCH_SIZE!=0) %>% 
          mutate(Mat_fem_tot = sum(SAMPLING_FACTOR, na.rm = T)) %>%
          mutate(clutch = ifelse(EGG_CONDITION==2, "Eyed",
                                 ifelse(EGG_CONDITION==4 | EGG_CONDITION==0 & CLUTCH_SIZE==1, "Barren",
                                        ifelse(EGG_CONDITION==5, "Hatching", NA)))) %>%
          filter(!is.na(clutch)) %>%
          summarise(`Total Barren/Hatching Females` = ((round(sum(SAMPLING_FACTOR,na.rm = T)))),
                    `Total Mature Females` = mean(Mat_fem_tot),
                    `Threshold (%)` = ((round(sum(SAMPLING_FACTOR,na.rm = T)))/mean(Mat_fem_tot))*100) %>%
          gt() %>%
          tab_header(
            title = "BBRKC Resampling Threshold",
            subtitle = paste0(stations_remaining, " stations remaining in BBRKC Mgmt District")) %>%
          tab_caption(caption = md(run_date)) %>%
          tab_style(locations = cells_body(columns = `Threshold (%)`),
                    style = cell_fill(color = "lightblue")) %>%
          opt_table_lines()

#Table 2: Threshold by clutch codes
table2 <- clutch_thresh %>%
          select(-total_mature, -thres_total) %>%
          rename(`% of Mature Females` = clutch_perc, `Clutch Code` = clutch,
                 Count = thresh_fem_sum) %>%
          gt() %>%
            tab_header(
              title = "BBRKC Resampling Threshold",
              subtitle = paste0(stations_remaining, " stations remaining in BBRKC Mgmt District")) %>%
            tab_caption(caption = md(run_date)) %>%
                grand_summary_rows(columns = `% of Mature Females`, 
                                 fns = list(label = "RUNNING THRESHOLD", id = "totals", fn = "sum")) %>%
              tab_style(locations = cells_grand_summary(),
                style = cell_fill(color = "lightblue")) %>% 
            opt_table_lines()
    
#Now combine tables and save
listed_tables <- list(table1, table2)
gt_two_column_layout(listed_tables, output = "save", 
                     filename = paste0(run_date, "_THRESHOLD.png"),
                      path = "./Output") 

