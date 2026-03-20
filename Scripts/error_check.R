# ERROR CHECKING -----------------------------------------------------------------------------------------------------
#
### ### ### ### ### ### ###
#**UNDER DEVELOPMENT!!!* #
### ### ### ### ### ### ###
#
# NOTES: ----
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


#### START HERE ##
# Load libraries ----
  library(tidyverse)
  library(purrr)
  library(cli) # for colored text
  library(stringi) # read .txt files
  library(data.table) # read .txt files??


# Set recorder, vessel/leg - user needs to do ----
# (set directories - will use vessel/leg)
  vessel <- "AKK" #"NWEx"
  leg <- "Leg1" #"Leg2"
  recorder <- "Shannon Hennessey"


##**maybe make a check within the function to make sure vessel/leg inputs are correct??*
##*would need to do this before setting the paths because they won't work if the leg/vessel is slightly off
  path <- "c:/Users/Shannon.Hennessey/Desktop/onboard error checks/"
  in_dir <- paste0(path, "QAQC_queue/")
  out_dir <- paste0(path, vessel, "/", leg, "/") # clean files, archive, error reports
  #**sFTP_dir?**


# Start global file checks ----
## compile temporary/placeholder "haul" file? ----
# - ID 0-catch stations
# - where do I want to draw this from?? just the files in the queue? or all hauls so far?


# # Read in ALL files in "to be checked" folder
# files_all <- 

# Read in all HAUL files first to make list of which hauls to be checked... 
# - extract haul #, tablet, and timestamp for use down the line....
  haul_info_all <- data.frame(FILE = list.files(in_dir, pattern = "_HAUL_", recursive = TRUE)) %>%
                   separate_wider_delim(FILE, "_HAUL", names = c("TABLET", "HAUL_NUMBER", "DATETIME")) %>%
                   mutate(DATETIME = str_sub(DATETIME, 2, 9),
                          TABLET_STR = str_remove(str_sub(TABLET, 1, 6), "_$")) %>%
                   # join with all unique tablet/timestamp combos in queue files to verify have a haul file for them all... 
                   # (which should be present for every good haul)
                   full_join(., data.frame(FILE = list.files(in_dir, recursive = TRUE)) %>%
                                   mutate(TABLET_STR = str_remove(str_sub(FILE, 1, 6), "_$"), 
                                          DATETIME = str_sub(FILE, -12, -5)) %>%
                                   select(-FILE) %>%
                                   distinct(),
                             by = join_by(DATETIME, TABLET_STR)) %>%
                   # order by haul#, tablet, timestamp:
                   mutate(haul_temp = as.numeric(HAUL_NUMBER),
                          datetime_temp = as.numeric(DATETIME)) %>%
                   arrange(haul_temp, TABLET, datetime_temp) %>%
                   select(-haul_temp, -datetime_temp)

# throw warning if there's a TABLET_STR/DATETIME combo that doesn't have a 'RAW_HAUL' file in the queue
  if(nrow(haul_info_all %>% filter(is.na(HAUL_NUMBER))) > 0){
      cat(col_red(paste0("'RAW_HAUL' files for the following tablet and timestamp combinations are missing from the QAQC queue folder,\nwhile other files with this combination are present:\n")))
      
    # Make object with tablet/timestamp combos with no RAW_HAUL file
      no_haul <- haul_info_all %>% filter(is.na(HAUL_NUMBER))
      
    # Print error for each tablet/timestamp combination 
      for(i in 1:nrow(no_haul)){
        cat(col_red(paste0("   - Tablet starting with '", no_haul$TABLET_STR[i], "' and timestamp '", no_haul$DATETIME[i], "'\n")))
      }
  
      cat(col_red(paste0("\nPlease double check the files in the QAQC queue folder and make sure all relevant tablet files are present before proceeding.\n")))
      cat(col_red(paste0("\nStopping the error checking protocol.\n\n")))

    #**some sort of break here if this check is automated??*
  }

  
#**popup to verify # and which hauls to be checked?? Ie. there are 4 hauls to be checked, correct?*
#  and if no, put a reminder to go back and make sure all files are there....
#  - something to look back/ID any sequential haul #s missing and verify that those are bad hauls?
#    (or maybe that's something I can do on my end with the temp haul file...)
# Option for "recheck" if made changes, and wouldn't run this check?

  haul_queue <- unique(haul_info_all$HAUL_NUMBER)
  
  if(length(haul_queue) == 0){
    cat(col_red("\nNo hauls were identified in the QAQC queue. Please go back and make sure the tablet files are in the appropriate folder.\n"))
  } else{
    cat(paste0("\nThe following ", length(haul_queue), " hauls are present in the QAQC queue:\n"))
    for(n in 1:length(haul_queue)){
      cat(paste0("   - Haul ", haul_queue[n], "\n"))
    }
    
    queue_check <- menu(c("Yes", "No"), title = "\nDoes this look correct?")
    
    if(queue_check == 1){
      cat("\nYou selected 'Yes'.\nProceeding with the error checking protocol.\n\n") #**maybe update this wording a bit?*
    }
    
    if(queue_check == 2){
      cat("\nYou selected 'No'.\n\n", sep = "")
      cat(col_red("Stopping the error checking protocol.\n", sep = ""))
      cat(col_red("Please review the files in the QAQC queue folder and ensure that the files for all intended hauls are present.\n\n"))
      
      break() #**something different to stop the flow?*
    }
  }



# Start haul-specific file checks ----
#**# Loop over hauls here....START OF ERROR CHECKING**
  # for(h in 1:length(hauls)){
  #   haul_number <- hauls[h] # haul_number <- unique(haul_info$HAUL_NUMBER)[h]
  # }

# Identify number of haul to be checked
  haul_number <- unique(haul_info_all$HAUL_NUMBER)[6] #**CHANGE THIS FOR FINAL*

# Create haul ID for output file naming
  haul_id <- str_remove(haul_number, "^0+")

# Subset the haul information to the relevant haul
  haul_info <- haul_info_all %>% 
               filter(HAUL_NUMBER == haul_number) #%>%
               # add_row(TABLET = "SAM 78", HAUL_NUMBER = "0143", DATETIME = "06301740") ##Added TEMPORARY/FAKE copies to detect duplicates!!

  
#**PRINT SOMETHING HERE for commentary?* "starting xx checks for haul xx...."


## Create error report template ----  
##**CREATE ERROR VECTOR/ITERATOR FOR HAUL*
  errors <- data.frame(ERROR_TYPE = character(),
                       ERROR_MESSAGE = character(),
                       stringsAsFactors = FALSE)


##**READ IN/CHECK FOR ERROR REPORT HERE??*
##  - Make note of if one exists, then have a "RE-CHECK" indicated in the file??
##  - MAKE NOTE IN PROTOCOL --> please don't rename things!!!!
##  - Will also need to collect some other haul/time info for report output...come up with header format
# report_file <- list.files(paste0(out_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul113"))
  report_file <- list.files(paste0(out_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul", str_sub(haul_number, 2, 4)))


  if(length(report_file) == 1){
    # read in existing error report file
      report <- read.delim(paste0(out_dir, "_error_reports/", report_file)) 
    
    #...then what...create "recheck" header for appending...
    
  } else if(length(report_file) == 0){
    # if it doesn't exist already, create new file....
    # need some standard things here for header etc. 
    # How do delim line breaks, blank lines, etc? can we preserve those with output/input through R?
    
  } else{
    # THROW ERROR - multiple files for same haul?? go back and look/reconcile....
    #**do I want to provide an option to move on to the next haul in the meantime??*
      cat(col_red("Multiple error report files were detected for Haul ", haul_id, ". \n   To correct this, please:\n"))
      cat(col_red("      (1) Review the files in the ", vessel, " ", leg, " 'Error Reports' folder;\n"))
      cat(col_red("      (2) Reconcile any differences and combine into one comprehensive error report file;\n"))
      cat(col_red("      (3) Delete the extra file(s), and try again.\n\n"))
      break()
  }



## check tablet duplicates ----
#**if same tablet/haul_number combo, pick the most recent datetime and archive the "older" files that have presumably been edited*
# NEED TO TEST IF MULTIPLE VERSIONS FROM MULTIPLE TABLETS...function and error messages
  if(nrow(haul_info %>% select(-DATETIME) %>% distinct()) < nrow(haul_info)){
      cat(col_red(paste0("\nDuplicate files for Haul ", haul_id, " were detected with different timestamps.\n")))
      
      move_selection <- menu(c("Yes", "No"), title = "\nWould you like to move the files with the earlier timestamp(s) to the 'Archive' folder?")
      
      if(move_selection == 1){
          cat("\nYou selected 'Yes'.\n\n")
      
        ## move extra/older files to the archive folder
    
        # ID tablet/timestamp that's duplicated, and which to move
          duplicate_files <- haul_info %>% 
                             group_by(TABLET) %>% 
                             mutate(N_TIMESTAMP = n()) %>%
                             filter(N_TIMESTAMP > 1)
          
          archive_tablets <- unique(duplicate_files$TABLET)
          archive_timestamps <- duplicate_files %>%
                                filter(!as.numeric(DATETIME) == max(as.numeric(DATETIME))) %>%
                                pull(DATETIME)
                    
          to_archive <- list.files(in_dir, pattern = archive_timestamps, recursive = TRUE) %>% # ok if multiple timestamps, would need to iterate... probs for multiple tablets too?
                        map(~file.rename(from = paste0(in_dir, .x),
                                         to = paste0(out_dir, "_archive/", .x)))
        
        # Update haul_info to remove archived files from the object
          haul_info <- haul_info %>% filter(!DATETIME %in% archive_timestamps)
          haul_info_all <- haul_info_all %>% filter(!DATETIME %in% archive_timestamps)
        
        # Add note to Error Report
          error_iter <- nrow(errors) + 1
          errors[error_iter, 1] <- "File"
          errors[error_iter, 2] <- paste0("Duplicate files were detected from tablet '", archive_tablets, "'. '", archive_tablets, "' files for Haul ", haul_id, " with the timestamp '", archive_timestamps, "' were moved to the ", vessel, " ", leg, " 'Archive' folder.")
      
        # Print message
          cat("Extra files for Haul ", haul_id, " have been moved to the ", vessel, " ", leg, " 'Archive' folder.\n\n", sep = "")
      }
      
      if(move_selection == 2){
          cat("\nYou selected 'No'.\n\n", sep = "")
          cat(col_red("Stopping the error checking protocol.\n", sep = ""))
          cat(col_red(paste0("Please review the tablet files for Haul ", haul_id, " and make sure only the most current versions are in the queue.\n\n", sep = "")))
          
          break()
      }
  }


# read in all files for first haul....
  files <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
#**also read in any previous error report??* can use file.append() to add on to it w/ the recheck
#*and maybe note somewhere that there's a recheck/edits made? indicate some sort of repeat section/header??

  
## Check station ID inputs? ----  
  
  
## ID any note files ----
  if(length(list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE)) > 0){
      notes <- list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE) %>%
               map_df(~read.csv(paste0(in_dir, .x))) %>%
               select(HAUL_ID, NOTE_TABLE, NOTES)
  }
# parse into specimen notes and xx notes by NOTE_TABLE??
# catch_sample_notes - bin subsample
# specimen_notes - rotten eggs, etc. 
# haul_notes - 0-catch station



# Start on first haul of ones in folder
# - if 0-catch, have a little popup Y/N to confirm 0-catch, if Y, outputs that 
#   as "error report" and moves to next haul

## ID potential zero-catch station ----
#**MAKE THIS A FUNCTION TO SOURCE??*
#*Also, do a 2nd iteration of this when just RAW_HAUL is present without a note?? 
#*  and verify 0-catch and say please add a note??
  if(length(files) == 2 & exists("notes")){
      no_catch_selection <- menu(c("Yes", "No"), title = paste0("\nWas Haul ", haul_id, " a zero-catch station?"))
    
      if(no_catch_selection == 1){
          move_selection <- menu(c("Yes", "No"), title = "\nYou selected 'Yes' - would you like to move on to the next haul?")
          
          if(move_selection == 1){
              cat("\nYou selected 'Yes'.\n\n", sep = "")
              
              ## OUTPUT ERROR REPORT HERE indicating 0-catch station -- make function for outputting error report and call here!!!!
              error_iter <- nrow(errors) + 1
              errors[error_iter, 1] <- "No Catch"
              errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
              
              ## MOVE FILES
              
              cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n", sep = "") 
              cat("Starting error checks for the next haul.\n\n", sep = "")
              
              next()
          }
          
          if(move_selection == 2){
              cat("\nYou selected 'No'.\n\n", sep = "")
              
              ## OUTPUT ERROR REPORT HERE indicating 0-catch station
              error_iter <- nrow(errors) + 1
              errors[error_iter, 1] <- "No Catch"
              errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
              
              ## MOVE FILES
              
              cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n", sep = "")
              cat(col_red("Stopping the error checking protocol.\n\n", sep = ""))
              
              break()
          }
      } else if(no_catch_selection == 2){
          cat("You selected 'No'.\n\n")
          cat(col_red("No 'CATCH' or 'SPECIMEN' files for Haul ", haul_id, " are present in the queue.\n", sep = ""))
          cat(col_red("Please make sure those files are in the 'QAQC_queue' folder and try again for this haul.\n\n"))
          
         #**PRINT ERROR REPORT*
          error_iter <- nrow(errors) + 1
          errors[error_iter, 1] <- "File"
          errors[error_iter, 2] <- paste0("No 'CATCH' or 'SPECIMEN' files are present in the QAQC queue")
          
          next_selection <- menu(c("Yes", "No"), title = "\nWould you like to move on to the next haul in the meantime?")
          
          if(next_selection == 1){
              cat("You selected 'Yes'.\n")
              cat("Saving Haul", haul_id, "error report and starting error checks for the next haul.\n\n")
              next()
          }
          
          if(next_selection == 2){ #**PRINT ERROR REPORT?*
              cat(col_red("You selected 'No'.\n"))
              cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
              break()
          }
    } # do I need a final "else" here? Selection not recognized, please run these lines again?
  }


## make sure all necessary files are present for the haul -- throw error if not
# (0-catch check will have moved on to next haul....so this should only need to flag incomplete hauls that aren't 0-catch)

## Inventory files for the given haul by tablet output type ----
  files_inventory <- list(files[grepl(paste0("_CRAB_CATCH_", haul_number), files)],
                          files[grepl(paste0("_CRAB_SPECIMEN_", haul_number), files)],
                          files[grepl(paste0(haul_number, "_HAUL_"), files)],
                          files[grepl(paste0(haul_number, "_SAMPLE_0"), files)],
                          files[grepl(paste0(haul_number, "_SAMPLE_VALUES_"), files)],
                          files[grepl(paste0(haul_number, "_SPECIMEN_0"), files)],
                          files[grepl(paste0(haul_number, "_SPECIMEN_BIOMETRICS_"), files)])


# Check if any tablet file type has fewer files than the rest (excluding NOTES files),
# and if so, flag which files are missing from the QA/QC queue/add to error report
  if(any(lengths(files_inventory) < max(lengths(files_inventory)))){
    
    # Print message(s) flagging which files are missing for the haul
      cat(col_red("\nA complete suite of files for Haul ", haul_id, " is not present in the queue:\n", sep = ""))
      
    # Make vector of file types for error messages
      file_type <- c("'CRAB_CATCH'", "'CRAB_SPECIMEN'", 
                     "'RAW_HAUL'", "'RAW_SAMPLE'", "'RAW_SAMPLE_VALUES'", 
                     "'RAW_SPECIMEN'", "'RAW_SPECIMEN_BIOMETRICS'")
    
    # Loop over file types to ID which are missing, if any
      for(i in 1:7){
        
        
        # If any files of that type are missing, print error message  
          if(length(files_inventory[[i]]) < max(lengths(files_inventory))){

            # Check which tablet/timestamp combos are missing 
              if(length(files_inventory[[i]]) == 0){
                missing_combos <- haul_info %>% 
                                  select(-HAUL_NUMBER, TABLET_STR)
              } 
            
              if(length(files_inventory[[i]]) > 0 & i %in% 1:2){
                missing_combos <- data.frame(TABLET = unlist(files_inventory[[i]] %>% map(~unlist(str_split(.x, "_CRAB_"))[1])),
                                             DATETIME = unlist(str_sub(files_inventory[[i]], -12, -5))) %>%
                                  mutate(PRESENT = 1) %>%
                                  full_join(., haul_info %>% select(-HAUL_NUMBER, TABLET_STR), by = join_by(TABLET, DATETIME))

              }
            
              if(length(files_inventory[[i]]) > 0 & i %in% 3:7){
                missing_combos <- data.frame(TABLET = unlist(files_inventory[[i]] %>% map(~unlist(str_split(.x, paste0("_HAUL", haul_number)))[1])),
                                             DATETIME = unlist(str_sub(files_inventory[[i]], -12, -5))) %>%
                                  mutate(PRESENT = 1) %>%
                                  full_join(., haul_info %>% select(-HAUL_NUMBER, -TABLET_STR), by = join_by(TABLET, DATETIME))
              }
              
            
            # Add note to Error Report??
            #**Think about this one...should we only add to error report if they want to move on to the next haul??*
            #*I guess if we just disregard and stop, all the errors would go away...which should be fine, unless some files were moved to archive?
            #*Maybe we print report regardless, but I guess only these file errors would be recorded if 
            
            #**add another loop for length of missing_tablet/missing_timestamp??
              for(m in 1:nrow(missing_combos)){
                  error_iter <- nrow(errors) + 1
                  errors[error_iter, 1] <- "File"
                  errors[error_iter, 2] <- paste0("A ", file_type[i], " file is missing from Tablet '", missing_combos$TABLET[m], "' with timestamp '", missing_combos$DATETIME[m], "'")
                
                # Print error message
                  cat(col_red(paste0("- A ", file_type[i], " file is missing from Tablet '", missing_combos$TABLET[m], "' with timestamp '", missing_combos$DATETIME[m], "'\n")), sep = "")
              }  
              
          } else{
            next()
          }
      }
      
      cat(col_red("\nPlease make sure the listed files are in the 'Queue' folder and try again for this haul.\n\n"))
      
      #**something here that allows continuing if a CATCH and SPECIMEN file are present, even if other RAW files are missing?*
      
      # Print menu to select whether or not to move onto the next haul or stop the error checking protocol
        next_selection <- menu(c("Yes", "No"), title = "Would you like to move on to the next haul in the meantime?")
        
        if(next_selection == 1){
          #**WRITE ERROR REPORT??* Think about if need 2 error report locations....
          #*one in QAQC as temp, and then a final one that gets moved once the haul is "approved"?
            cat("\nYou selected 'Yes'.\n\n", sep = "")
            cat("Saving Haul ", haul_id, " error report.\n", sep = "") 
            cat("Starting error checks for the next haul.\n\n", sep = "")
            # cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
            next()
        }
        
        if(next_selection == 2){
          #**WRITE ERROR REPORT??*
            cat(col_red("You selected 'No' - stopping the error checking protocol.\n\n"))
            break()
        }
  }



## Start catch/specimen checks ----
# Designate "catch" and "specimen" dfs to be checked
#**re-calculate/combine catch numbers by species....sum/double check the rounding on #specimens* 
#*-- should automatically take care of 2 tablet scenarios...
  specimen <- list.files(in_dir, pattern = paste0("_CRAB_SPECIMEN_", haul_number), recursive = TRUE) %>%
              map_df(~read.csv(paste0(in_dir, .x)))
    
  catch <- list.files(in_dir, pattern = paste0("_CRAB_CATCH_", haul_number), recursive = TRUE) %>%
           map_df(~read.csv(paste0(in_dir, .x))) %>%
           group_by(VESSEL, CRUISE, HAUL, STATION, COMMON_NAME, SPECIES_CODE) %>%
           # combine weights and catch numbers by species (if 2 tablets were used for the haul)
           summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE),
                     NUMBER_CRAB = sum(NUMBER_CRAB, na.rm = TRUE), 
                     .groups = "drop_last")
           # Need to check specimen #s should only be off by 1, right (if 2 tablets)??




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
#   -- how to disregard errors so they don't come up again in recheck....might be too tricky for now?
  cat(paste0("Printing error summary for Haul ", haul_id, "...\n\n" ))
  print(errors)

# message Y/N move to clean folder? Copy to thumb drive??
# - need directories for "clean", thumb drive backup, to FTP folder?
# - also need a message/reminder to double check that the files went where you think they did!!
  
  cat(paste0("Please review the errors for Haul ", haul_id, ".\n" ))
  cat(paste0("If there is nothing that needs correcting, please select 'Yes' when prompted to output the final error report and move the tablet files to their final folders.\n" ))
  cat(paste0("If corrections for Haul ", haul_id, " are needed, please select 'No'.\n\n" ))
  
  ok_selection <- menu(c("Yes", "No"), title = paste0("Is Haul ", haul_id, " clean and ready to be moved out of the 'QAQC_queue' folder?\n"))
  
  if(ok_selection == 1){
    #**WRITE ERROR REPORT HERE* 
    
    #**MOVE FILES*
    to_clean <- list.files(in_dir, pattern = archive_timestamps, recursive = TRUE) %>% # ok if multiple timestamps, would need to iterate... probs for multiple tablets too?
                map(~file.rename(from = paste0(in_dir, .x),
                                 to = paste0(out_dir, "_archive/", .x)))
  
    
    cat("\nYou selected 'Yes'.\n\n", sep = "")
    cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n\n", sep = "") 

  
    # Move on to next haul?
    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul?")
    
    if(next_selection == 1){
      cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
      next()
    }
    
    if(next_selection == 2){
      cat(col_red("You selected 'No' - stopping the error checking protocol.\n\n"))
      break()
    }
  }
  
  if(ok_selection == 2){
    #**WRITE ERROR REPORT*
    
    cat(col_red("You selected 'No'.\n"))
    cat(col_red("Please review the errors in the error report and make changes in tablet as needed.\n"))
    cat(col_red("When the errors have been corrected, please put the re-extracted files into the 'QAQC_queue' folder and run them through the error checking protocol again. Don't forget to annotate any changes in the existing error report for Haul ", haul_id, " as well.\n\n"))

    
    ##**MOVE FILES TO ARCHIVE??*
    archive_selection <- menu(c("Yes", "No"), title = paste0("Would you like to archive the existing tablet files for Haul ", haul_id, " ?"))
      
    if(archive_selection == 1){
      
      # to_clean <- list.files(in_dir, pattern = archive_timestamps, recursive = TRUE) %>% # ok if multiple timestamps, would need to iterate... probs for multiple tablets too?
      #             map(~file.rename(from = paste0(in_dir, .x),
      #                              to = paste0(out_dir, "_archive/", .x)))
                
      cat("You selected 'Yes'.\n")
      cat("Tablet files for Haul ", haul_id, " have been moved to the ", vessel, " ", leg, " 'Archive' folder.\n\n", sep = "")
    }
    
    if(archive_selection == 2){
      cat(col_red("You selected 'No'.\n"))
      cat(col_red("Extra files for Haul ", haul_id, " have not been moved. Please make sure to move these files out of the 'QAQC_queue' folder before running error checks on this haul again.\n\n"))
    }

    
    # Move on to next haul?
    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul in the meantime?")
    
    if(next_selection == 1){
      cat("You selected 'Yes'.\n")
      cat("Saving Haul ", haul_id, " error report and starting error checks for the next haul.\n\n", sep = "")
      next()
    }
    
    if(next_selection == 2){
      cat(col_red("You selected 'No'.\n"))
      cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
      break()
    }
    
  }
  
  
  
  
  # next_selection <- menu(c("Yes", "No"), title = "Would you like to move on to the next haul in the meantime?")
  # 
  # if(next_selection == 1){
  #   #**WRITE ERROR REPORT??* Think about if need 2 error report locations....
  #   #*one in QAQC as temp, and then a final one that gets moved once the haul is "approved"?
  #   cat("\nYou selected 'Yes'.\n\n", sep = "")
  #   cat("Saving Haul ", haul_id, " error report.\n", sep = "") 
  #   cat("Starting error checks for the next haul.\n\n", sep = "")
  #   # cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
  #   next()
  # }
  # 
  # if(next_selection == 2){
  #   #**WRITE ERROR REPORT*
  #   
  #   cat(col_red("You selected 'No'.\n"))
  #   cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
  #   break()
  # }

# Y/N move on to next haul?



# At the end: ----

#**STOP -- take a look at the error report. Do you need to make any changes??*
#*If no, do you want to move the files?? if yes, do you want to archive the existing files for the haul??


#**Then at very end* after all hauls are good: ----
# re-compile catch and specimen db tables!
# - will need to read from the vessel/leg "clean" folders with a different directory...
#   but the db's will *ONLY* compile from the "clean" folder (by leg)
# - should I have a pre-check to make sure there are no duplicate files in the clean??
#   - only check duplicate timestamps for haul/tablet, might have 2 tablets for a haul still...
  specimen_db <- list.files(paste0(out_dir, "Crab SPECIMEN Files/"), pattern = paste0("_CRAB_SPECIMEN_"), recursive = TRUE) %>%
                 map_df(~read.csv(paste0(out_dir, "Crab SPECIMEN Files/", .x))) %>%
                 write.csv(., paste0(out_dir, "SPECIMEN_db.csv"), row.names = FALSE)
  
  catch_db <- list.files(paste0(out_dir, "Crab CATCH Files/"), pattern = paste0("_CRAB_CATCH_"), recursive = TRUE) %>%
              map_df(~read.csv(paste0(out_dir, "Crab CATCH Files/", .x))) %>%
              group_by(VESSEL, CRUISE, HAUL, STATION, COMMON_NAME, SPECIES_CODE) %>%
              # combine weights and catch numbers by species (if 2 tablets were used for the haul)
              summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE),
                        NUMBER_CRAB = sum(NUMBER_CRAB, na.rm = TRUE), 
                        .groups = "drop_last") %>%
              # # summarize catch numbers by species from specimen table and update catch numbers 
              # # if there were rounding discrepancies from using 2 tablets 
              # # (ie. tablet rounds to whole numbers but if the catch was split, 0.4 and 0.4 round down, but 0.8 rounds up)
              # #**do we round back to whole NUMBER_CRAB??*
              # left_join(., specimen_db %>%
              #              group_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE) %>%
              #              summarise(CATCH = sum(SAMPLING_FACTOR)),
              #           by = join_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE)) %>%
              # mutate(NUMBER_CRAB = ifelse(CATCH > NUMBER_CRAB, round(CATCH), NUMBER_CRAB)) %>% # will the specimen #s always be larger??
              # # and really should only be off by 1, right?? hmm think about more
              # select(-CATCH) %>%
              write.csv(., paste0(out_dir, "CATCH_db.csv"), row.names = FALSE)
#**.^^ DO WE WANT THIS AUTOMATICALLY COMBINED HERE? ^^.* Would have to drop the HAUL_ID, RECORDING_DEVICE, and ID columns...
# - Think about if this matters for downstream data things....hypothetically I could use the compiled DBs from each leg 
#   to run final checks and go straight to Oracle?





# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# SPECIMEN CHECK FUNCTION ----
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
    
    
   ##**need to incorporate crab species into this!!*
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
    
    
    ##**need to incorporate crab species into this!!*
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
    
    
    ##**need to incorporate crab species into this!!*
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

  
  
  
  
  
  
  
  
  
  
  
  
