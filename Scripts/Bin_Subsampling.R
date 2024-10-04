#Goals: Evaluate the efficacy of a bin subsampling protocol that estimates total catch weight 
#of small Chionoecetes at any given station on the EBS bottom trawl survey. Specifically:
  #1) Calculate sampling factor for a given haul (two methods)
  #2) Estimate total crab catch weight of C.mix via a sampling factor and table subsample of crab catch
  #3) Visually compare observed C.mix catch weight vrs estimated catch weight 

#Author: Erin Fedewa

#load packages
library(tidyverse)

#load AKK Leg 3 data 
dat <- read.csv("./Data/Bin_subsampling_dat.csv")

#Calculate sampling factors and estimate total catch 
dat %>%
  mutate(catch_weight = bag_weight-net_weight,
    #Method 1: Using subsample catch weight
      sf1 = catch_weight/(sub_weight-17.95), #empty brailer = 17.95kg
    #Method 2: Using whole hauled vrs subsampled species weights
      sf2 = (catch_weight-wh_crab_weight-wh_fish_weight)/(cmix_table_weight+sub_fish_weight),
    #Estimate total catch 
      catch_sf1 = cmix_table_weight*sf1,
      catch_sf2 = cmix_table_weight*sf2,
    #Calculate ratio between estimated and observed C.mix catch weight 
      catch_ratio_sf1 = catch_sf1/cmix_total_weight,
      catch_ratio_sf2 = catch_sf2/cmix_total_weight) -> sf_est

#Is estimated C.mix catch weight consistently under/overestimated?
#If there is systematic bias in subsampling from the bin, can we apply a correction factor? 

#Plot ratio by catch size 
sf_est %>%
  ggplot() +
  geom_point(aes(catch_weight, catch_ratio_sf1, color="red"), size=3) +
  geom_point(aes(catch_weight, catch_ratio_sf2, color="blue"), size=3) +
  geom_abline(slope=0, intercept=1) +
  labs(y="Estimated/Observed C.Mix Catch Weight Ratio", x="Total Catch Weight (kg)") +
  scale_color_identity(name = "Sampling Factor",
                       breaks = c("red", "blue"),
                       labels = c("Subsample weight", "Non-sub:sub weight"),
                       guide = "legend") +
  theme_bw()

#Plot ratio by C. mix crab catch size 
sf_est %>%
  ggplot() +
  geom_point(aes(cmix_total_weight, catch_ratio_sf1), color="red", size=3) +
  geom_point(aes(cmix_total_weight, catch_ratio_sf2), color="blue", size=3) +
  geom_abline(slope=0, intercept=1) +
  labs(y="C.Mix Catch Weight Ratio", x="Total C.Mix Catch Weight (kg)") +
  theme_bw()

#qqplot: observed vrs estimated catch for sampling factor method 1
sf_est %>%
  select(cmix_total_weight, catch_sf1, catch_sf2) %>%
  pivot_longer(c(2:3), names_to = "sampling_factor_method", values_to = "estimated_catch") %>%
  ggplot(aes(cmix_total_weight, estimated_catch, color=sampling_factor_method)) +
  geom_point(size=3) +
  theme_bw() +
  labs(x="Observed C.mix catch weight", y="Estimated C.mix catch weight", 
       color="Sampling Factor Method") +
  geom_abline(intercept =0 , slope = 1)

