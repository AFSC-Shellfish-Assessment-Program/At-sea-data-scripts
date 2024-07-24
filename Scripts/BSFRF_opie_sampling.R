# Goal: Output 1mm binned data for snow crab males Females >90mm CL
# Generated for BSFRF for 2024 snow crab pilot study:
# 1) 1mm binned counts for all snow crab males in shelf stratum 61 and slope subarea 6


# NOTE: Using most recent FTP'd data only- Products are preliminary until QA/QC! 
# This script should ONLY be used during the survey, as haul files have 
# not yet been created to filter out special project tows by haul type

#Author: S. Hennessey

#load
library(tidyverse)

################

#Set path for most recent FTP'd data on Kodiak server 
path <- "Y:/KOD_Survey/EBS Shelf/2024/RawData/"

#Now read in all specimen tables from FTP'd data 
dat <- list.files(path, pattern = "CRAB_SPECIMEN", recursive = TRUE) %>% 
       purrr::map_df(~read.csv(paste0(path, .x)))

# # Designate special project hauls/bad tows to drop until haul types are assigned (see resampling script); bad tows probably won't matter because there isn't data associated with them
# AKK_drop <- as.data.frame(c(1:17,23,25,45, 124, 133:140)) %>% setNames("HAUL")   #124 bad tow for AKK     
# NWE_drop <- as.data.frame(c(1:16,21,23,28:29,39, 121, 123:124)) %>% setNames("HAUL")  #121, 123:124 bad tows for NWE

# Create Look up table for shelf stratum 61 and slope subarea 6 stations (39 potential stations) 
# Only 6 of shelf stratum 61 stations will have a side-by-side tow (but include catches from all standard stations),
# and 13 slope subarea 6 stations will be sampled (with side-by-side tows)
stations <- data.frame("R-32", "R-31", "Q-31", "P-32", "P-31", "O-31", "N-31", "M-32", "M-31", "L-31",
                       "R-32-B", "R-31-B", "Q-31-B", "P-32-B", "P-31-B", "O-31-B", "N-31-B", "M-32-B", "M-31-B", "L-31-B",
                       "61-15", "61-14", "61-18", "61-13", "61-12", "61-21", "61-07", "61-05", "61-06", "61-22", 
                       "61-20", "61-03", "61-04", "61-17", "61-02", "61-01", "61-08", "61-09", "61-16") 
                       # should there be subarea 6 "-B" stations?? I think so...

#Standardize station naming, filter for stations 
data <- dat %>%
        filter(str_detect(STATION, "-")) %>% #additional filter to remove any corner stations
        #Standardize station name notation to ensure there were no station name tablet entry errors
        separate(STATION, sep = "-", into = c("col", "row", "tow")) %>%
        dplyr::select(-tow) %>%
        mutate(row = str_pad(row, width = 2, pad = "0")) %>% #make sure all names have leading zeros
        unite("STATION", col:row, sep = "-") %>%
        filter(STATION %in% stations)
# Ignore the warning message:
# "Expected 3 pieces. Missing pieces filled with `NA`"

# Compute sum of all males by 1mm size bin   
male_sizecomp <- data %>%
                 filter(SPECIES_CODE == 68580,
                        SEX == 1) %>%
                 # create 1mm size bin
                 mutate(SIZE_BIN = floor(LENGTH)) %>%
                 # create zero catch size bins
                 right_join(expand_grid(SIZE_BIN = min(.$SIZE_BIN):max(.$SIZE_BIN))) %>%
                 replace_na(list(SAMPLING_FACTOR = 0)) %>%
                 # calculate total crab for each size bin
                 group_by(SIZE_BIN) %>%
                 summarise(TOTAL_CRAB = sum(SAMPLING_FACTOR)) 

write.csv(male_sizecomp, file="./Output/BSFRF_opie_Allmale_Size_Comps.csv", row.names=FALSE)

