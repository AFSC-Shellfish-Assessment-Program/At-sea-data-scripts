# Goals of at-sea data scripts

## This repository contains script to assess the following while the eastern and northern Bering Sea bottom trawl surveys are underway: 
- The proportion of female red king crab having not yet completed the molt/mate cycle. If 10% of mature females have not yet molted/mated, a resampling will be considered at 20 stations in Bristol Bay _(BBRKC_Resampling_Survey.R)_  

- Look up historical catch data for a given station and/or year of interest using the GAPsurvey R package _(Get catch history.R)_    

- Develop preliminary at-sea estimates of the number of snow crab and tanner crab caught. These estimates are sent to stock assessment authors as updates throughout the EBS survey _(Snow Crab Count.R)_  
  
- Test a pilot special project addressing the efficacy of a bin subsampling protocol. This pilot special project was run on Legs 3 and 4 of the 2023 bottom trawl survey _(Bin Subsampling.R)_  


## Shellfish Assessment Program Resampling Protocol
In colder years, a portion of the female Bristol Bay red king crab population has not yet completed the molt-mate cycle when Bristol Bay is surveyed during the NMFS eastern Bering Sea bottom trawl survey.  Under these conditions, a subset of the Bristol Bay stations has historically been resampled later in the summer to improve the accuracy of the size composition data for post-molt Bristol Bay red king crab females. Secondary goals are to assess the reproductive status and abundance of these females after the molt-mate cycle is complete. 

A resampling in Bristol Bay is considered if 10% or more of the mature females in the Bristol Bay District have not yet completed the molt-mate cycle.  Females are considered to have an incomplete molt-mate cycle if they have any of the following clutch characteristics: 1) eyed eggs [x-2-x], 2) hatching eggs [0-5-0], 3) empty egg cases [0-4-1], or 4) barren; mature with no eggs [0-0-1].  Shell condition is NOT considered, although shell condition 0 - 2 females counting towards the threshold have molted, but not yet mated. These females may weigh into decisions to resample, as improving size composition data remain the #1 priority for resampling. Females that have completed the molt-mate cycle have uneyed eggs. 

If a resampling is necessary, it will occur at 20 stations within the Bristol Bay District.  Resample stations are selected based on the density of mature females during the initial sampling event.  The resampling area must include stations containing 80% of mature females with an incomplete molt-mate cycle sampled during the original survey, with consideration of the total mature female distribution.  Priority is given to a contiguous block of resample stations, so long as the above criteria can be met.

A revised 25% threshold was proposed to the NPFMC in June 2022, however, the SSC recommended retaining the 10% threshold. See https://github.com/AFSC-Shellfish-Assessment-Program/Resampling-Protocol for additional analyses relevant to resampling decisions. 
