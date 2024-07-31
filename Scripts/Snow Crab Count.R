#Goal: Calculate current year running sum of snow and tanner crab 
  #caught on EBS BT survey 
#NOTE: These are preliminary counts and have not been QA/QC'd! 

#Author: Erin Fedewa

library(RODBC)
library(tidyverse)
library(ggridges)
library(viridis) 
library(patchwork)
options(scipen = 999) #remove scientific notation from ggplot axes

#Step 1: Download catch data
#a) Download final access database for each vessel AFTER survey leg is completed 
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
AKK_L1 <- import("./Data/DataFromBoats/AKKN_2024_Leg1.accdb")
AKK_L2 <- import("./Data/DataFromBoats/AKKN_2024_Leg2.accdb")
AKK_L3 <- import("./Data/DataFromBoats/AKKN_2024_Leg3.accdb")
#AKK_L4 <- import("./Data/DataFromBoats/AKKN_2024_Leg4.accdb")
NWX_L1 <- import("./Data/DataFromBoats/NWE_2024_Leg1.accdb")
NWX_L2 <- import("./Data/DataFromBoats/NWE_2024_Leg2.accdb")
NWX_L3 <- import("./Data/DataFromBoats/NWE_2024_Leg3.accdb")

#Step 3: Drop non-standard stations (15:30 and slope/shelf)
AKK_drop <- as.data.frame(c(1:17,23,25,45,133:140,152,154,169,170,209)) %>% setNames("HAUL")           
NWE_drop <- as.data.frame(c(1:16,21,23,28:29,39,145,147,160,161)) %>% setNames("HAUL") 

#Step 4: Merge data frames and sum number of snow crab caught (from tablet)
bind_rows(AKK_L1, AKK_L2, AKK_L3, NWX_L1, NWX_L2, NWX_L3) %>% 
  filter(SPECIES_CODE == 68580,
         CRUISE == 202401) %>%
  filter(!(VESSEL == 162 & HAUL %in% AKK_drop$HAUL),
                !(VESSEL == 134 & HAUL %in% NWE_drop$HAUL)) %>%
  summarise(Total = sum(NUMBER_CRAB)) -> snow24 #220694 snow crab

#Step 4: Merge data frames and sum number of tanner crab caught (from tablet)
bind_rows(AKK_L1, AKK_L2, AKK_L3, NWX_L1, NWX_L2, NWX_L3) %>% 
  filter(SPECIES_CODE == 68560,
         CRUISE == 202401) %>%
  filter(!(VESSEL == 162 & HAUL %in% AKK_drop$HAUL),
         !(VESSEL == 134 & HAUL %in% NWE_drop$HAUL)) %>%
  summarise(Total = sum(NUMBER_CRAB)) -> tanner24 #38697 tanner crab

#Quick plot 
#Append new data to timeseries 
n <- read.csv("./Data/crab_n_timeseries.csv") %>% select(Year, Snow_n, Tanner_n)
new <- tibble(Year=2024, Snow_n=snow24[,1], Tanner_n=tanner24[,1])
n %>%
  full_join(new) -> crab_n

#goofy work-around for plotting breaks
crab_n %>%
  mutate(section1 = if_else(Year < 2020, TRUE, FALSE),
         section2 = if_else(Year %in% c(2019:2021), TRUE, FALSE),
         section3 = if_else(Year > 2020, TRUE, FALSE),
         section5 = if_else(Year < 2024, TRUE, FALSE),
         section6 = if_else(Year == 2024, TRUE, FALSE)) %>%
  filter(!is.na(Snow_n)) -> df

#Snow crab total count plot
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
  scale_x_continuous(breaks = 2012:2024) -> snow

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
  scale_x_continuous(breaks = 2012:2024) -> tanner

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
AKK_L1_s <- import2("./Data/DataFromBoats/AKKN_2024_Leg1.accdb")
AKK_L2_s <- import2("./Data/DataFromBoats/AKKN_2024_Leg2.accdb")
AKK_L3_s <- import2("./Data/DataFromBoats/AKKN_2024_Leg3.accdb")
NWX_L1_s <- import2("./Data/DataFromBoats/NWE_2024_Leg1.accdb")
NWX_L2_s <- import2("./Data/DataFromBoats/NWE_2024_Leg2.accdb")
NWX_L3_s <- import2("./Data/DataFromBoats/NWE_2024_Leg3.accdb")

#filter by males > 101mm 
bind_rows(AKK_L1_s, AKK_L2_s, AKK_L3_s, NWX_L1_s, NWX_L2_s, NWX_L3_s) %>% 
  filter(SPECIES_CODE == 68580,
         SEX == 1, 
         WIDTH >= 102,
         CRUISE == 202401) %>%
  filter(!(VESSEL == 162 & HAUL %in% AKK_drop$HAUL),
         !(VESSEL == 134 & HAUL %in% NWE_drop$HAUL)) %>%
  count() -> pref #878 males 

#Number of PIBKC
bind_rows(AKK_L1_s, AKK_L2_s, AKK_L3_s, NWX_L1_s, NWX_L2_s, NWX_L3_s) %>% 
  filter(SPECIES_CODE == 69323,
         c(grepl(pattern="K|J|I|H|G|F|E",STATION))) %>%
  count() -> bkc #9

#Plot for Buck and Cody 
n <- read.csv("./Data/crab_n_timeseries.csv")
new2 <- tibble(Year=2024, Snow_101_n=pref[,1], Prib_BKC=bkc[,1])
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

#Size Frequency distribution plots - snow crab 
bind_rows(AKK_L1_s, AKK_L2_s, AKK_L3_s, NWX_L1_s, NWX_L2_s, NWX_L3_s) %>%  
  filter(SPECIES_CODE == 68580,
         CRUISE == 202401) %>%
  filter(!(VESSEL == 162 & HAUL %in% AKK_drop$HAUL),
         !(VESSEL == 134 & HAUL %in% NWE_drop$HAUL)) %>%
  mutate(year = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  ggplot() +
  geom_density(aes(WIDTH)) +
  scale_fill_viridis(name = "CW (mm)", option = "C") +
  theme_bw() +
  labs(x="Snow Crab Carapace Width", y="") -> snow_size

#Size Frequency distribution plots - tanner crab 
bind_rows(AKK_L1_s, AKK_L2_s, AKK_L3_s, NWX_L1_s, NWX_L2_s, NWX_L3_s) %>%  
  filter(SPECIES_CODE == 68560,
         CRUISE == 202401) %>%
  filter(!(VESSEL == 162 & HAUL %in% AKK_drop$HAUL),
         !(VESSEL == 134 & HAUL %in% NWE_drop$HAUL)) %>%
  mutate(year = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  ggplot() +
  geom_density(aes(WIDTH)) +
  scale_fill_viridis(name = "CW (mm)", option = "C") +
  theme_bw() +
  labs(x="Tanner Crab Carapace Width", y="") -> tanner_size

snow_size/tanner_size
