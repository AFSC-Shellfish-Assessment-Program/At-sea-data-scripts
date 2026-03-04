# ERROR CHECKING -----------------------------------------------------------------------------------------------------
#
##########################
#**UNDER DEVELOPMENT!!!* #
##########################
#
# NOTES:
# - need to make a way to detect and bind catch/specimen from 2 tablets, 
#   but also make sure they're from 2 different tablets, not the same one re-extracted
# 
# - maybe do a function?? and for each haul, print a preliminary error report (ie.
#   "no errors" or "this is a 0-catch station, correct?") with a Y/N option, and then
#   ask if you want to move the files to the "clean" folder....that way can say N
#   for ones that you need to go back to but clear the ones that are good
# - and then once fixed, re-error check? and have that append to the separate 
#   error report for that haul (I think we need separate ones so there can be an
#   organized followup that says good to go...can we get it to read old errors
#   and have the user automatically say yes this is ok??)

# - look into txt file interfacing w/ R....or maybe .csv??


#**Write up a step-by-step narrative in code comments!*


## Check 0-catch stations and correct haul-station combos needs to be done separate
## but can verify vessel/cruise #s in on-board script

# Compare total catch vs. estimated weights by species...

# How to append .txt files??
#**What's the best method for disseminating code fixes if we need some?? GitHub? Just via email??*

# Move clean files to permanent folder
# Re-generate CATCH and SPECIMEN master files

# Lines to also move to FTP folder? need to think about the mechanics of this more....

#**Send out a poll to crab team to gauge preferences for QA/QC script functionality and outputs?*
# - function (with lots of messages/informative errors and TESTED) vs. script?
# - error report by haul or combined? (I prefer individual ones/combined by repeat checks...)
# - would any other haul-level summaries be helpful?


######################
# Load libraries
library(tidyverse)
library(purrr)
library(cli) # for colored text


# Set recorder, vessel/leg - user needs to do
# (set directories - will use vessel/leg)

vessel <- "AKK" #"NWEx"
leg <- "Leg1" #"Leg2"
recorder <- "Shannon Hennessey"


##**maybe make a check within the function to make sure vessel/leg inputs are correct??*
##*would need to do this before setting the paths because they won't work if the leg/vessel is slightly off
path <- "c:/Users/Shannon.Hennessey/Desktop/onboard error checks/"
in_dir <- paste0(path, "QA_QC/1_queue/")
out_dir <- paste0(path, vessel, "/", leg, "/")


# # Read in ALL files in "to be checked" folder
# files_all <- 

# Read in all HAUL files first to make list of which hauls to be checked...
# - extract haul #, tablet, and timestamp for use down the line....
haul_info_all <- data.frame(FILE = list.files(in_dir, pattern = "_HAUL_", recursive = TRUE)) %>%
                 separate_wider_delim(FILE, "_HAUL", names = c("TABLET", "HAUL_NUMBER", "DATETIME")) %>%
                 mutate(DATETIME = str_sub(DATETIME, 2, 9))


#**popup to verify # and which hauls to be checked?? Ie. there are 4 hauls to be checked, correct?*
#  and if no, put a reminder to go back and make sure all files are there....
#  - something to look back/ID any sequential haul #s missing and verify that those are bad hauls?
#    (or maybe that's something I can do on my end with the temp haul file...)
# Option for "recheck" if made changes, and wouldn't run this check?

# # Loop over hauls here....
# for(h in 1:length(hauls)){
#   haul_number <- hauls[h] # haul_number <- unique(haul_info$HAUL_NUMBER)[h]
# }

# Identify number of haul to be checked
haul_number <- unique(haul_info_all$HAUL_NUMBER)[1] #**CHANGE THIS FOR FINAL*

# Create haul ID for output file naming
haul_id <- str_remove(haul_number, "^0+")

# Subset the haul information to the relevant haul
haul_info <- haul_info_all %>% 
             filter(HAUL_NUMBER == haul_number) %>%
             add_row(TABLET = "SAM 78", HAUL_NUMBER = "0143", DATETIME = "06301740")


#**if same tablet/haul_number combo, pick the most recent datetime??*
#*need to figure out how to integrate this/modify the total list of files..
#*and then archive the "older" files that have presumably been edited? 
if(nrow(haul_info %>% select(-DATETIME) %>% distinct()) < nrow(haul_info)){
  cat(col_red(paste0("\nDuplicate files for Haul ", haul_id, " were detected with different timestamps.\n")))
  
  move_selection <- menu(c("Yes", "No"), title = "\nWould you like to move the files with the earlier timestamp to the 'Archive' folder?")
  
  if(move_selection == 1){
    cat("\nYou selected 'Yes'.\n\n", sep = "")
  
    ## move extra/older files to the archive folder
    #**make a list of the files to move, and then maybe have to apply/loop this rename to each of the files?*
    # file.rename(from = paste0(in_dir, spec_file),
    #             to = paste0(out_dir, "_archive/", spec_file))
    
    cat("Extra files for Haul ", haul_id, " have been moved to the ", leg, " 'Archive' folder.\n", sep = "")
  }
  
  if(move_selection == 2){
    cat("\nYou selected 'No'.\n\n", sep = "")
    cat(col_red("Stopping the error checking protocol.\n", sep = ""))
    cat(col_red(paste0("Please review the files for Haul ", haul_id, " and make sure only the most current versions are in the queue.\n\n", sep = "")))
    
    break()
  }
}


# read in all files for first haul....
files <- list.files(in_dir, pattern = haul_number, recursive = TRUE)

# ID any note files
if(length(list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE)) > 0){
  notes <- list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE) %>%
           purrr::map_df(~read.csv(paste0(in_dir, .x))) %>%
           select(HAUL_ID, NOTE_TABLE, NOTES)
}


# ID potential zero-catch station
#**MAKE THIS A FUNCTION TO SOURCE??*
if(length(files) == 2 & exists("notes")){
  no_catch_selection <- menu(c("Yes", "No"), title = paste0("\nWas Haul ", haul_id, " a zero-catch station?"))
  
  if(no_catch_selection == 1){
    move_selection <- menu(c("Yes", "No"), title = "\nYou selected 'Yes' - would you like to move on to the next haul?")
    
    if(move_selection == 1){
      cat("\nYou selected 'Yes'.\n\n", sep = "")
      cat("Saving Haul ", haul_id, " error report and moving files to the 'clean' folder.\n", sep = "") ## make better message for "clean"
      cat("Starting error checks for the next haul.\n\n", sep = "")
      
      ## OUTPUT ERROR REPORT HERE indicating 0-catch station
      ## MOVE FILES
      next()
    }
    
    if(move_selection == 2){
      cat("\nYou selected 'No'.\n\n", sep = "")
      cat("Saving Haul ", haul_id, " error report and moving files to the 'clean' folder.\n", sep = "") ## make better message for "clean"
      cat(col_red("Stopping the error checking protocol.\n\n", sep = ""))
      
      ## OUTPUT ERROR REPORT HERE indicating 0-catch station
      ## MOVE FILES
      break()
    }
  } else if(no_catch_selection == 2){
    cat("You selected 'No'.\n\n")
    cat(col_red("No 'CATCH' or 'SPECIMEN' files for Haul ", haul_id, " are present in the queue.\nPlease make sure those files are in the appropriate folder and try again for this haul.\n\n", sep = ""))
    
    next_selection <- menu(c("Yes", "No"), title = "\nWould you like to move on to the next haul in the meantime?")
    
    if(next_selection == 1){
      cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
      next()
    }
    
    if(next_selection == 2){
      cat(col_red("You selected 'No' - stopping the error checking protocol.\n\n"))
      break()
    }
  } # do I need a final "else" here? Selection not recognized, please run these lines again?
}


## make sure all necessary files are present for the haul -- throw error if not
# (0-catch check will have moved on to next haul....so this should only need to flag incomplete hauls that aren't 0-catch)

#**check if 2 tablets were used first!!*
# - using AKK haul 143 as tester



if(!length(files) %in% c(7,8)){
  cat(col_red("\nA complete suite of files for Haul ", haul_id, " is not present in the queue.\nPlease make sure those files are in the appropriate folder and try again for this haul.\n\n", sep = ""))
  
  next_selection <- menu(c("Yes", "No"), title = "\nWould you like to move on to the next haul in the meantime?")
  
  if(next_selection == 1){
    cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
    next()
  }
  
  if(next_selection == 2){
    cat(col_red("You selected 'No' - stopping the error checking protocol.\n\n"))
    break()
  }
}


# parse into specimen notes and xx notes by NOTE_TABLE??
# catch_sample_notes - bin subsample
# specimen_notes - rotten eggs, etc. 
# haul_notes - 0-catch station




# compile temporary/placeholder "haul" file 
# - ID 0-catch stations
# - where do I want to draw this from?? just the files in the queue? or all hauls so far?


# Start on first haul of ones in folder
# - if 0-catch, have a little popup Y/N to confirm 0-catch, if Y, outputs that 
#   as "error report" and moves to next haul

# Check for/combine multiple tablets used for same haul 
# - append specimens, sum catch weights by species? (but maybe after checking catch vs. estimated weights)


# Designate "catch" and "specimen" dfs to be checked
catch <- list.files(in_dir, pattern = paste0("_CRAB_CATCH_", haul_number), recursive = TRUE) %>%
         purrr::map_df(~read.csv(paste0(in_dir, .x)))
specimen <- list.files(in_dir, pattern = paste0("_CRAB_SPECIMEN_", haul_number), recursive = TRUE) %>%
            purrr::map_df(~read.csv(paste0(in_dir, .x)))


# Across all specimens:
# - all specimens have a length/width (and correct measurement type for correct species)
# - # specimen biometrics matches # in catch summary? (using sampling factor I think)
# - correct species, sex codes
# - nonsensical clutch codes
# - clutch/no for male/female
# - catch weight vs. estimated weights? 
#   - this might be hard, because catch summary is @ species level, and we can't match to subsample for sure....(size/sex categories)
#   - maybe just continue to rely on tablet checks for this one....
# - any specimens above/below 99th %ile size? (or some way to set bounds/ID outliers....)
# - flag any species never seen at a given station before?? (have to make sure we have "-B" stations defined for modernization hauls...)
#**have an iterator for error messages so every time it throws one, those save as object and get complied/printed in final .txt file?*
#**...incorporate this into the 0-catch station check too, and maybe the files # check?*

# For each species:
# - small mature females (especially mature barren RKC/BKC)
# - RKC: shell 3:5 and EC 1



# summary of any notes associated with haul (e.g. saw rotting clutches, etc.)

# output summary, say will pop up, check it then verify if good/not

# message Y/N move to clean folder?
# - need directories for "clean", thumb drive backup, to FTP folder?

# Y/N move on to next haul?



## At the end:
# re-compile catch and specimen db tables!
# - will need to read from the vessel/leg "clean" folders with a different directory...








# ------------------------------------------------------------------------------
# Write function
error_chk <- function(method, specimen_table, catch_summary, potlifts, haul, cpue){
  
  print("CHECKING VESSEL AND CRUISE...")
  
  # 1) Does cruise number match 202501?
    if(FALSE %in% unique(specimen_table$CRUISE == 202301)){
      print("ERROR: wrong cruise number")
    }
  
  
  # 2) Does the vessel # match the vessels utilized in the survey?

      if(FALSE %in% (unique(specimen_table$VESSEL) %in% c("Vesteraalen")) == TRUE){
        print("ERROR: vessel numbers entered do not match survey vessels")
      }
  
    
    print("Inventory of catch by cruise and vessel")
    invent1 <- specimen_table %>%
      dplyr::group_by(VESSEL, CRUISE) %>%
      dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                     N = n()) %>%
      as.data.frame()
    print(invent1)
  
  
  print("CHECKING SPECIES CODES AND SEX...")
  
  
 ## ** SH NOTE: need to incorporate crab species into this!! ** ----------
  # 3) Do species codes match RKC code 69322?"
    if(FALSE %in% unique(specimen_table$SPECIES_CODE == 69322)){
      print("ERROR: wrong code entered for RKC")
    }
  
  
  # 4) Are sex codes assigned to either 1 or 2?"
    if(unique(specimen_table$SEX %in% c(1:2)) == FALSE){
      print("ERROR: sex code not 1 or 2")
    }
  
    print("Inventory of catch by cruise, vessel, species code, and sex")
    invent2 <- specimen_table %>%
      dplyr::group_by(VESSEL, CRUISE, SPECIES_CODE, SEX) %>%
      dplyr::reframe(COUNT = sum(SAMPLING_FACTOR), 
                     N = n()) %>%
      as.data.frame()
    print(invent2)
  
  
  print("CHECKING SHELL CONDITION, EGG COLOR, EGG CONDITION, CLUTCH_SIZE...")
  
  # 5) Are egg color codes valid for females?"
    if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$EGG_COLOR %in% c(0, 2:6))) == TRUE){
      print("ERROR: invalid female egg color code (not 0 or 2:6)")
    }
  
  
  # 6) Are egg condition codes valid for females?"
    if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$EGG_CONDITION %in% c(0:5))) == TRUE){
      print("ERROR: invalid female egg condition code (not 0:5)")
    } 
  
  
  # 7) Are clutch size codes valid for females?
    if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$CLUTCH_SIZE %in% c(0:6, 999))) == TRUE){
      print("ERROR: invalid female clutch size code (not 0:6)")
    }
  
  
  # 8) Any egg, egg condition, or clutch size codes assigned to males?
    if(TRUE %in% (unique((specimen_table$SEX == 1 & is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION | 
                                                          specimen_table$CLUTCH_SIZE) == FALSE))) == TRUE){
      print("ERROR: egg, egg condition, or clutch size code assigned to male")
    } 
    
  
  # 9) Any questionable egg condition x shell condition combinations for females?"
  # Checking shell condition = 0 and egg condition = 1
    if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 0 & 
                         filter(specimen_table, SEX != 1)$EGG_CONDITION == 1)) == TRUE){
      print("ERROR: female with shell condition = 0 and egg condition = 1")
    } 
  
  # Checking shell condition = 1 and egg condition >1
    if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 1 & 
                         filter(specimen_table, SEX != 1)$EGG_CONDITION > 1)) == TRUE){
      print("ERROR: female with shell condition = 1 and egg condition >1")
    }
  
  # Checking shell condition = 3, 4, or 5 and egg condition = 1
    if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION %in% c(3:5) & 
                         filter(specimen_table, SEX != 1)$EGG_CONDITION == 1)) == TRUE){
      print("ERROR: female with shell condition 3:5 and egg condition = 1")
    }
  
  # Checking shell condition = 1 and egg condition >=2
    if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 1 & 
                         filter(specimen_table, SEX != 1)$EGG_CONDITION >= 2)) == TRUE){
      print("ERROR: female with shell condition = 1 and egg condition >=2")
    }
    
  
  # 10) Any females without egg color, egg condition, or clutch codes?
    if(TRUE %in% (unique(specimen_table$SEX == 2 & (is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION 
                                                          | specimen_table$CLUTCH_SIZE) == TRUE))) == TRUE){
      print("ERROR: female missing egg color, egg condition, or clutch code")
    }
  
  
    print("Inventory of shell condition")
    invent3 <- specimen_table %>%
      dplyr::group_by(SPECIES_CODE, SEX, SHELL_CONDITION) %>%
      dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                     N = n()) %>%
      as.data.frame()
    print(invent3)
    
    print("Inventory of female shell condition and egg codes")
    invent4 <- specimen_table %>%
      dplyr::filter(SEX == 2) %>%
      dplyr::group_by(SPECIES_CODE, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, CLUTCH_SIZE) %>%
      dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                     N = n()) %>%
      as.data.frame()
    print(invent4)
  
  
  print("CHECKING CRAB SIZES...")
  
  
 ## ** SH NOTE: need to incorporate crab species into this!! ** ----------
  # 11) Any missing lengths for RKC?"
    if(unique(is.na(specimen_table$LENGTH)) == TRUE){
      print("ERROR: missing length for RKC")
    } 
  
  
  # 12) Any small female crab with a clutch size?"
    if(unique(filter(specimen_table, SEX != 1)$LENGTH < 65 & 
              filter(specimen_table, SEX != 1)$CLUTCH_SIZE > 0) == TRUE){
      print("ERROR: female <65 with clutch size >0")
    } 
  
  
  # 13) Any small crab with old shell condition?"
    if(TRUE %in% unique(specimen_table$LENGTH < 60 & specimen_table$SHELL_CONDITION > 2)){
      print("ERROR: crab < 60 with shell condition >2")
    } 
  
  ## PLOT SIZE/WEIGHT FOR OUTLIERS?? 0 weights?
  ## flag entries of sizes +_ 2 sd LW regression estimate?
  
  
 ## ** SH NOTE: need to incorporate crab species into this!! ** ----------
  # 14) Any widths entered for RKC? 
    if(unique(is.na(specimen_table$WIDTH)) == FALSE) {
      print("ERROR: width entered for RKC when length is needed")
    }
  
  
  # 15) Minimum and maximum lengths by sex?
    print("What are the minimum and maximum lengths reported by sex?")
    A15 <- specimen_table %>%
           dplyr::group_by(SPECIES_CODE, SEX) %>%
           dplyr::reframe(MIN_LENGTH = min(LENGTH),
                          MAX_LENGTH = max(LENGTH)) %>%
           as.data.frame()
    print(A15)
  
  
  print("CHECKING DISEASE CODES...")
  
  # 16) Any black mat recorded but without % coverage entry?
    if(unique(is.na(specimen_table$DISEASE_CODE) == FALSE &
              specimen_table$DISEASE_CODE == 1 & (is.na(specimen_table$DISEASE_DORSAL) == TRUE &
                                                  is.na(specimen_table$DISEASE_LEGS) == TRUE &
                                                  is.na(specimen_table$DISEASE_VENTRAL) == TRUE)) == TRUE){
      print("ERROR: black mat recorded without % coverage")
    } 
  
  
  # 17) Any bitter crab recorded for RKC and/or any bitter crab recorded with entries in % coverage?
    if(unique(is.na(specimen_table$DISEASE_CODE) == FALSE & specimen_table$DISEASE_CODE == 2 |
              is.na(specimen_table$DISEASE_CODE) == FALSE & specimen_table$DISEASE_CODE == 2 & 
              (is.na(specimen_table$DISEASE_DORSAL) == FALSE &
               is.na(specimen_table$DISEASE_LEGS) == FALSE &
               is.na(specimen_table$DISEASE_VENTRAL) == FALSE)) == TRUE){
      print("ERROR: bitter crab recorded for RKC and/or bitter crab recorded with % coverage")
    } 
  
  
  # 18) Any disease code not recorded but entries in % coverage?
    if(unique(is.na(specimen_table$DISEASE_CODE) == TRUE & (is.na(specimen_table$DISEASE_DORSAL) == FALSE &
                                                            is.na(specimen_table$DISEASE_LEGS) == FALSE &
                                                            is.na(specimen_table$DISEASE_VENTRAL) == FALSE)) == TRUE){
      print("ERROR: disease code not recorded but % cover entered")
    } 
  
  
  # 19) Any disease codes >9? 
    if(unique(is.na(specimen_table$DISEASE_CODE) == "FALSE" & specimen_table$DISEASE_CODE > 9) == TRUE){
      print("ERROR: disease code >9")
    }
  
  
  print("CHECKING SAMPLING FACTOR...")
  
  # 20) What is the maximum sampling factor by by sex?
    print("What is the maximum sampling factor by sex?")
    A20 <- specimen_table %>%
           dplyr::group_by(SPECIES_CODE, SEX) %>%
           dplyr::reframe(MIN_SAMPLING_FACTOR = min(SAMPLING_FACTOR),
                          MAX_SAMPLING_FACTOR= max(SAMPLING_FACTOR)) %>%
           as.data.frame() 
    print(A20)
  
    
  # 21) Any sampling factors < 1?"
    if(unique(specimen_table$SAMPLING_FACTOR < 1) == TRUE){
      print("ERROR: minimum sampling factor < 1")
    } 
  
  
  print("CHECKING CRUISE, HAUL, AND STATION IDs...")
      xx <- cpue %>%
            dplyr::select(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD) %>%
            distinct() %>%
            as.data.frame()
      
      yy <- potlifts %>%
            dplyr::select(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD) %>%
            distinct()
      
      if(TRUE %in% is.na(suppressMessages(right_join(xx, yy, keep = TRUE))) == TRUE){
        print("ERROR: POT: pot, station, and/or buoy IDs do not match between potlifts table and pot cpue table")
      }

      
  
  
  print("COMPARING SPECIMEN TABLE WITH CATCH SUMMARY...")
  
  # 22) Does the number of crab and number of entries match between the specimen table and catch summary?
    if(method == "POT"){
      spec_sum <- specimen_table %>%
                  dplyr::group_by(CRUISE, VESSEL, POT_ID, SPECIES_CODE) %>%
                  dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR),
                                 N_ENTRIES = n())
      
      catch_sum <- catch_summary %>%
                   dplyr::select(CRUISE, VESSEL, POT_ID, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES)
      
      if(TRUE %in% is.na(suppressMessages(right_join(spec_sum, catch_sum))) == TRUE){
        print("ERROR: POT: number of crab and number of entries do not 
                 match between summarized specimen table and catch summary")
      }
    } 

    if(method == "TRAWL"){
      spec_sum <- specimen_table %>%
        dplyr::group_by(CRUISE, VESSEL, HAUL, SPECIES_CODE) %>%
        dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR),
                       N_ENTRIES = n())
      
      catch_sum <- catch_summary %>%
        dplyr::select(CRUISE, VESSEL, HAUL, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES)
      
      if(TRUE %in% is.na(suppressMessages(right_join(spec_sum, catch_sum))) == TRUE){
        print("ERROR: TRAWL: number of crab and number of entries do not 
                   match between summarized specimen table and catch summary")
      } 
    }
  
  
  # print("CHECKING COORDINATES...")
  # 
  #   # Transform potlifts data to correct crs
  #   mapdat <- potlifts %>%
  #             sf::st_as_sf(coords = c(x = "LON_DD", y = "LAT_DD"), crs = sf::st_crs(4326)) %>%
  #             sf::st_transform(crs = map.crs)
  #   
  #   # Map coordinates
  #   coords <- ggplot() +
  #             geom_sf(data = st_as_sf(BB_strata), fill = NA, color = "black", linewidth = 1) +
  #             geom_sf(data = st_as_sf(RKCSA_sub), fill = NA, color = "red", alpha = 0.5, linewidth = 1) +
  #             geom_sf(data = st_as_sf(RKCSA), fill = NA, color = "red", alpha = 0.5, linewidth = 1) +
  #             geom_sf(data = mapdat, shape = 19, size = 1.5, colour = "black", stat = "identity", 
  #                     position = "identity")+
  #             ggtitle("Pot coordinate check")+
  #             theme_bw()
  #   
  #   print(coords)

}    


