#Goal: Calculate 2022 running sum of snow crab caught on EBS BT survey 
  #Would be really awesome if we had a database like Metis that would enable pulling
    #of all survey leg data simultaneously real time vrs access databases
    # for each leg...but until that happens, here's a clunky script to track down our data

#Author: Erin Fedewa
#Last run: 7/29/22 using final EBS FTP  

library(RODBC)
library(tidyverse)

#Step 1: Download catch data
  #a) Download final access database for each vessel AFTER survey leg is completed 
#M:\EBS Shelf\2022\RawData (I copied Access databases manually per leg/boat)
  #b) Download most recent FTP'd access database from ONGOING leg for both vessels 
#https://sftp.afsc.noaa.gov/ThinClient/WTM/public/index.html#/login
  
#Step 2: Import catch table only from each access database (TO DO: create loop for this!)
  db1 <- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/AKKN_2022_Leg1.accdb"
  con1 <- odbcConnectAccess2007(db1)
  sqlTables(con1)$TABLE_NAME
  AKK_L1 <- sqlFetch(con1, "tblCRAB_CATCH")
  
  db2 <- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/AKKN_2022_Leg2.accdb"
  con2 <- odbcConnectAccess2007(db2)
  AKK_L2 <- sqlFetch(con2, "tblCRAB_CATCH")
  
  db3 <- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/AKKN_2022_Leg3.accdb"
  con3 <- odbcConnectAccess2007(db3)
  AKK_L3 <- sqlFetch(con3, "tblCRAB_CATCH")
  
  db4 <- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/VEST_2022_Leg1.accdb"
  con4 <- odbcConnectAccess2007(db4)
  VEST_L1 <- sqlFetch(con4, "tblCRAB_CATCH")
  
  db5<- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/VEST_2022_Leg2.accdb"
  con5 <- odbcConnectAccess2007(db5)
  VEST_L2 <- sqlFetch(con5, "tblCRAB_CATCH")
  
  db6<- "C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/VEST_2022_Leg3.accdb"
  con6 <- odbcConnectAccess2007(db6)
  VEST_L3 <- sqlFetch(con6, "tblCRAB_CATCH")
  
#Step 3: Merge data frames and sum number of snow crab caught (from tablet)
bind_rows(AKK_L1, AKK_L2, AKK_L3, VEST_L1, VEST_L2, VEST_L3) %>% 
  filter(SPECIES_CODE == 68580) %>%
  summarise(Total = sum(NUMBER_CRAB)) #76,273 snow crab

#FOR REFERENCE (as calculated per Table 5 in past tech memos)
#In 2021, total # of snow crab caught was: 
  10015 + 2095 + 839 + 27922 #40,871
#In 2019, total # of snow crab caught was: 
  76872 + 4775 + 3848 + 75800 #161,295
#In 2018, total # of snow crab caught was: 
  202255 + 3976 + 85265 + 123887  #415,383
  
#Step 4: Merge data frames and sum number of tanner crab caught (from tablet)
  bind_rows(AKK_L1, AKK_L2, AKK_L3, VEST_L1, VEST_L2, VEST_L3) %>% 
    filter(SPECIES_CODE == 68560) %>%
    summarise(Total = sum(NUMBER_CRAB)) #14,979 tanner crab
  
#FOR REFERENCE (as calculated per Table 5 in past tech memos)
#In 2021, total # of tanner crab (E&W) caught was: 
   1801+303+828+520+5166+687+3406+1593 #14304
#In 2019, total # of tanner crab (E&W) caught was: 
    1301+318+1059+132+5744+753+5736+1325 #16368
    
#Quick plot for 2022 ACCAP Presentation
sc <- read.csv("C:/Users/erin.fedewa/Work/Tech Memo/At sea data scripts/Data/SC_n_timeseries.csv")

sc %>%
  mutate(section1 = if_else(Year < 2020, TRUE, FALSE),
         section2 = if_else(Year %in% c(2019:2021), TRUE, FALSE),
         section3 = if_else(Year > 2020, TRUE, FALSE),
         section5 = if_else(Year < 2022, TRUE, FALSE),
         section6 = if_else(Year == 2022, TRUE, FALSE)) %>%
  filter(!is.na(sc_caught))-> df

  df %>%
    ggplot(aes(Year, sc_caught)) +
    geom_point(data = df %>% filter(section5 == TRUE), color = "blue", size=3) +
    geom_point(data = df %>% filter(section6 == TRUE), shape = 21, fill = "lightgray",
               color = "black", size = 5) +
    geom_line(data = df %>% filter(section1 == TRUE), color= "blue") +
    geom_line(data = df %>% filter(section2 == TRUE), color= "blue", linetype = "dashed") +
    geom_line(data = df %>% filter(section3 == TRUE), color= "blue") +
    theme_bw() +
    labs(y = "Number of crab") +
    theme(legend.position = "none", axis.title.x = element_blank()) +
     scale_x_continuous(breaks = 2012:2022)


  