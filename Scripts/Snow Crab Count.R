#Goal: Calculate 2023 running sum of snow crab caught on EBS BT survey 
#Would be really awesome if we had a database like Metis that would enable pulling
#of all survey leg data simultaneously real time vrs 6 access databases
# ...but until that happens, here's a clunky script to track down our data

#Author: Erin Fedewa
#Last run: 7/26/23, end of Leg 3

library(RODBC)
library(tidyverse)
library(patchwork)
options(scipen = 999) #remove scientific notation from ggplot axes

#Step 1: Download catch data
#a) Download final access database for each vessel AFTER survey leg is completed 
#M:\EBS Shelf\2022\RawData (I copied Access databases manually per leg/boat)
#b) Download most recent FTP'd access database from ONGOING leg for both vessels 
#https://sftp.afsc.noaa.gov/ThinClient/WTM/public/index.html#/login

#Step 2: Import catch table only from each access database 
#Function to import catch tables 
import <- function(filename) {
  con1 <- odbcConnectAccess2007(filename)
  sqlTables(con1)$TABLE_NAME
  sqlFetch(con1, "tblCRAB_CATCH")
}

#Add all databases
AKK_L1 <- import("./Data/DataFromBoats/AKKN_2023_Leg1.accdb")
AKK_L2 <- import("./Data/DataFromBoats/AKKN_2023_Leg2.accdb")
AKK_L3 <- import("./Data/DataFromBoats/AKKN_2023_Leg3.accdb")
NWX_L1 <- import("./Data/DataFromBoats/NWE_2023_Leg1.accdb")
NWX_L2 <- import("./Data/DataFromBoats/NWE_2023_Leg2.accdb")
NWX_L3 <- import("./Data/DataFromBoats/NWE_2023_Leg3.accdb")

#Step 3: Merge data frames and sum number of snow crab caught (from tablet)
bind_rows(AKK_L1, AKK_L2, AKK_L3, NWX_L1, NWX_L2, NWX_L3) %>% 
  filter(SPECIES_CODE == 68580) %>%
  summarise(Total = sum(NUMBER_CRAB)) -> snow23 #72,005 snow crab

#FOR REFERENCE (as calculated per Table 5 in past tech memos)
#This approach is cringe-worthy, I know....
#In 2022, total # of snow crab caught was:
23431 + 1730 + 35851 + 18591 #79,603
#In 2021, total # of snow crab caught was: 
10015 + 2095 + 839 + 27922 #40,871
#In 2019, total # of snow crab caught was: 
76872 + 4775 + 3848 + 75800 #161,295
#In 2018, total # of snow crab caught was: 
202255 + 3976 + 85265 + 123887  #415,383

#Step 4: Merge data frames and sum number of tanner crab caught (from tablet)
bind_rows(AKK_L1, AKK_L2, AKK_L3, NWX_L1, NWX_L2, NWX_L3) %>% 
  filter(SPECIES_CODE == 68560) %>%
  summarise(Total = sum(NUMBER_CRAB)) -> tanner23 #33,208 tanner crab

#FOR REFERENCE (as calculated per Table 5 in past tech memos)
#In 2022, total # of tanner crab (E&W) caught was:
2161+499+1381+345+4791+593+3668+1240 #14678
#In 2021, total # of tanner crab (E&W) caught was: 
1801+303+828+520+5166+687+3406+1593 #14304
#In 2019, total # of tanner crab (E&W) caught was: 
1301+318+1059+132+5744+753+5736+1325 #16368

#Quick plot 
#Append new data to timeseries 
n <- read.csv("./Data/crab_n_timeseries.csv")
new <- tibble(Year=2023, Snow_n=snow23[,1], Tanner_n=tanner23[,1])
n %>%
  full_join(new) -> crab_n

#goofy work-around for plotting breaks
crab_n %>%
  mutate(section1 = if_else(Year < 2020, TRUE, FALSE),
         section2 = if_else(Year %in% c(2019:2021), TRUE, FALSE),
         section3 = if_else(Year > 2020, TRUE, FALSE),
         section5 = if_else(Year < 2023, TRUE, FALSE),
         section6 = if_else(Year == 2023, TRUE, FALSE)) %>%
  filter(!is.na(Snow_n)) -> df

#Snow crab plot
df %>%
  ggplot(aes(Year, Snow_n)) +
  geom_point(data = df %>% filter(section5 == TRUE), color = "blue", size=3) +
  geom_point(data = df %>% filter(section6 == TRUE), shape = 21, fill = "lightgray",
             color = "black", size = 5) +
  geom_line(data = df %>% filter(section1 == TRUE), color= "blue") +
  geom_line(data = df %>% filter(section2 == TRUE), color= "blue", linetype = "dashed") +
  geom_line(data = df %>% filter(section3 == TRUE), color= "blue") +
  theme_bw() +
  labs(y = "Number of crab") +
  ggtitle("Snow Crab") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  scale_x_continuous(breaks = 2012:2023) -> snow

#Tanner crab plot
df %>%
  ggplot(aes(Year, Tanner_n )) +
  geom_point(data = df %>% filter(section5 == TRUE), color = "blue", size=3) +
  geom_point(data = df %>% filter(section6 == TRUE), shape = 21, fill = "lightgray",
             color = "black", size = 5) +
  geom_line(data = df %>% filter(section1 == TRUE), color= "blue") +
  geom_line(data = df %>% filter(section2 == TRUE), color= "blue", linetype = "dashed") +
  geom_line(data = df %>% filter(section3 == TRUE), color= "blue") +
  theme_bw() +
  labs(y = "Number of crab") +
  ggtitle("Tanner Crab") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  scale_x_continuous(breaks = 2012:2023) -> tanner

#combine plots
tanner/snow

######

#Number of industry preferred male snow crab 

#import specimen table function 
import2 <- function(filename) {
  con1 <- odbcConnectAccess2007(filename)
  sqlTables(con1)$TABLE_NAME
  sqlFetch(con1, "tblCRAB_SPECIMEN")
}

#Add all databases
AKK_L1_s <- import2("./Data/DataFromBoats/AKKN_2023_Leg1.accdb")
AKK_L2_s <- import2("./Data/DataFromBoats/AKKN_2023_Leg2.accdb")
AKK_L3_s <- import2("./Data/DataFromBoats/AKKN_2023_Leg3.accdb")
NWX_L1_s <- import2("./Data/DataFromBoats/NWE_2023_Leg1.accdb")
NWX_L2_s <- import2("./Data/DataFromBoats/NWE_2023_Leg2.accdb")
NWX_L3_s <- import2("./Data/DataFromBoats/NWE_2023_Leg3.accdb")

#filter by males > 101mm 
bind_rows(AKK_L1_s, AKK_L2_s, AKK_L3_s, NWX_L1_s, NWX_L2_s, NWX_L3_s) %>% 
  filter(SPECIES_CODE == 68580,
         SEX == 1, 
         WIDTH > 101) %>%
  count() -> pref #838 males > 101 vrs. 1000 in 2022, 

#Number of PIBKC
bind_rows(AKK_L1_s, AKK_L2_s, AKK_L3_s, NWX_L1_s, NWX_L2_s, NWX_L3_s) %>% 
  filter(SPECIES_CODE == 69323,
         c(grepl(pattern="K|J|I|H|G|F|E",STATION))) %>%
  count() -> bkc #9

#Plot for Buck and Cody 
n <- read.csv("./Data/crab_n_timeseries.csv")
new2 <- tibble(Year=2023, Snow_101_n=pref[,1], Prib_BKC=bkc[,1])
n %>%
  full_join(new2) -> crab_n

#Snow crab industry preferred plot
crab_n %>%
  filter(Year >= 2017) %>%
  ggplot(aes(Year, Snow_101_n)) +
  geom_point(size=3) +
  theme_bw() +
  labs(y = "Number of crab") +
  ggtitle("Snow Crab > 101") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none", axis.title.x = element_blank())  -> snow

#PIBKC plot
crab_n %>%
  filter(Year >= 2017) %>%
  ggplot(aes(Year, Prib_BKC)) +
  geom_point(size=3) +
  theme_bw() +
  labs(y = "Number of crab") +
  ggtitle("PIBKC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none", axis.title.x = element_blank())  -> blue

#combine plots
blue/snow
