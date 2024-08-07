# Goal: Output 1mm binned data for snow crab males Females >90mm CL
# Generated for BSFRF for 2024 snow crab pilot study:
# 1) 1mm binned counts for all snow crab males in shelf stratum 61 and slope subarea 6


# NOTE: Using most recent FTP'd data only- Products are preliminary until QA/QC! 
# This script should ONLY be used during the survey, as haul files have 
# not yet been created to filter out special project tows by haul type

# Author: S. Hennessey

# Load packages
library(tidyverse)

################

# Set path for most recent FTP'd data on Kodiak server 
path <- "Y:/KOD_Survey/EBS Shelf/2024/RawData/"

# Now read in all specimen tables from FTP'd data 
dat <- list.files(path, pattern = "CRAB_SPECIMEN", recursive = TRUE) %>% 
       purrr::map_df(~read.csv(paste0(path, .x)))

# Read in haul metadata 
haul <- read.csv("./Output/BSFRF_opilio_sampling/BSFRF_opie_haul_metadata.csv")


# Visually check stations
# sort(unique(dat$STATION))

# Create Look up table for shelf stratum 61 and slope subarea 6 stations (39 potential stations) 
# Only 6 of shelf stratum 61 stations will have a side-by-side tow (but include catches from all standard stations),
# and 13 slope subarea 6 stations will be sampled (with side-by-side tows)
stations <- data.frame("R-32", "R-31", "Q-31", "P-32", "P-31", "O-31", "N-31", "M-32", "M-31", "L-31",
                       "R-32-B", "R-31-B", "Q-31-B", "P-32-B", "P-31-B", "O-31-B", "N-31-B", "M-32-B", "M-31-B", "L-31-B",
                       "61-15", "61-14", "61-18", "61-13", "61-12", "61-21", "61-07", "61-05", "61-06", "61-22", 
                       "61-20", "61-03", "61-04", "61-17", "61-02", "61-01", "61-08", "61-09", "61-16",
                       "61-15-B", "61-14-B", "61-18-B", "61-13-B", "61-12-B", "61-21-B", "61-07-B", "61-05-B", "61-06-B", "61-22-B", 
                       "61-20-B", "61-03-B", "61-04-B", "61-17-B", "61-02-B", "61-01-B", "61-08-B", "61-09-B", "61-16-B") 

# Standardize station naming, filter for stations 
data <- dat %>%
        filter(STATION %in% stations) %>%
        filter(SPECIES_CODE == 68580,
               SEX == 1) %>%
        select(VESSEL, HAUL, STATION, SPECIES_CODE, SEX, WIDTH, SHELL_CONDITION, SAMPLING_FACTOR) %>%
        mutate(VESSEL = case_when(VESSEL == 162 ~ "AKK",
                                  VESSEL == 134 ~ "NWE"))
        # Ignore the warning message:
        # "Expected 3 pieces. Missing pieces filled with `NA`"

haul2 <- data %>%
         select(HAUL, STATION, SPECIES_CODE, SEX, WIDTH, SAMPLING_FACTOR) %>%
         right_join(., haul %>% select(-c(N_OPILIO_MALE))) %>%
         group_by(STATION, VESSEL) %>%
         summarise(N_OPILIO_MALE = sum(SAMPLING_FACTOR)) %>%
         replace_na(list(N_OPILIO_MALE = 0)) %>%
         left_join(haul %>% select(-c(N_OPILIO_MALE)), .)

write.csv(haul2, file="./Output/BSFRF_opilio_sampling/BSFRF_opie_haul_metadata.csv", row.names=FALSE)


# Compute sum of all males by 1mm size bin   
male_sizecomp <- data %>%
                 # create 1mm size bin
                 mutate(SIZE_BIN = floor(WIDTH)) %>%
                 # create zero catch size bins
                 # right_join(expand_grid(SIZE_BIN = min(.$SIZE_BIN):max(.$SIZE_BIN))) %>%
                 # replace_na(list(SAMPLING_FACTOR = 0)) %>%
                 # calculate total crab for each size bin
                 group_by(STATION, SIZE_BIN) %>%
                 summarise(TOTAL_CRAB = sum(SAMPLING_FACTOR)) %>%
                 arrange(STATION, SIZE_BIN)

write.csv(male_sizecomp, file="./Output/BSFRF_opilio_sampling/BSFRF_opie_allmale_size_comps.csv", row.names=FALSE)

# WOULD IT BE HELPFUL TO POOL counts by station for side-by-side, rather than keep tows separate?

