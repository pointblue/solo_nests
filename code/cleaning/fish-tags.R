# CORRECTED FISH-TAGGED NEST SUCCESS


# setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)

source('code/functions.R')

# get fish-tagged nest success from 1617, 1718, 1819, and 1920
rawFish <- 
  read_csv('data/FTCHXcombo.csv') %>% 
  # remove distant years
  filter(season != 1213 & season != 1314) %>% 
  # separate 'begin' into number of eggs
  mutate(begin = case_when(
    begin == '8E' ~ 8,
    begin == '2E' ~ 2,
    begin == '1E' ~ 1
  ),
  begin = if_else(nuch == 2,
                  2,
                  begin),
  begin = if_else(begin == 8,
                  1,
                  begin))

# Load data on all monitored GDR birds (2016-2018)
all.gdr <- 
  read_csv('data/croz_royds_gdr_depl_all_v2021-08-27.csv') %>% 
  # select only birds from crozier
  filter(br_col == 'CROZ') 

# historic nest success -----------------------------------------------------------

# identify known nest success for each bird (unobserved == FAIL)
succFish <- rawFish %>% 
  # find CR'd nests
  filter(grepl('CR', result)) %>%
  
  # convert CR1? case to CR1 confirmed after checking nest documentation
  mutate(result = if_else(result == 'CR1?',
                          'CR1',
                          result)) %>%
  # separate CR status from number of chx from each nest
  separate(result, 
           into = c("result", "number"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%  # pull(number) %>% is.na() %>% sum()
  
  # replace 2 NAs w. appropriate number
  mutate(number = as.numeric(number),
         correctNumb = if_else(is.na(number),
                               2,
                               number)) %>%
  # replace 8s with 1s after manual review
  mutate(correctNumb = if_else(correctNumb == 8,
                               1,
                               correctNumb)) %>% #pull(correctNumb)
  
  # add the failed nests back into the mix
  rbind(
    rawFish %>%  
      filter(!grepl('CR', result)) %>%
      mutate(number = 0, 
             correctNumb = 0)) %>% #group_by(season, location) %>% summarize(n = n())
  
  # add brood success (FinalCHsize >= GF) to the datatable
  mutate(FinalCHsize = 
           factor(FinalCHsize,
                  levels = c('UND', 'OR', 'GF', 'MEL', 'PA', 'PA+'),
                  ordered = T),
         BrSucc = if_else(
           FinalCHsize >= 'GF', # if we change this to GF criteria, BrSucc becomes larger than CrSucc (as we might expect)
           1,
           0)) %>% 
  # for each nest in each season, how many chx survived to CR?
  ## NOTE: Taking the mean but entries should be the same for each nest
  ### quality check for any non-integer values
  group_by(season, bandnumb) %>%
  summarise(crChx = mean(correctNumb), nuChx = n(), nuEgg = mean(begin), kaChx = sum(BrSucc)) %>% # pull(crChx) %>% unique()
  
  # get rid of a very strange case (random ch likely stumbled into nest)
  filter(nuChx != 3) %>% 
  distinct() %>%  # group_by(season) %>% summarize(seasonMean = mean(crChx))
  ungroup() %>% 
  mutate(season_yr = sapply(season, seas_fy)) 

# find and add all deviced birds monitored but not fish-tagged
## note that some of these birds may have been tagged, but just not succeeded
notnTagGDR <- 
  all.gdr %>% 
  dplyr::select(season_yr = season, bandnumb = bird_id, breeder, success) %>%
  # select breeders which failed to creche any chicks
  filter(breeder == 1 & success == 0) %>% 
  # add birds for 2019
  rbind(
    all.gdr %>%
      filter(season == 2018) %>% 
      dplyr::select(season_yr = season, bandnumb = bird_id, breeder = breeder_next, success = success_next) %>% 
      filter(breeder == 1 & success == 0) %>% 
      mutate(season_yr = 2019)) %>% 
  # remove any nests which were fish-tagged
  left_join(
    (succFish %>% dplyr::select(season_yr, bandnumb, crChx, kaChx)),
    by = c('season_yr', 'bandnumb')) %>% 
# our 6 failures are identified here minus 1 which was marked as success = 9 for 2019
filter(is.na(crChx)) %>%
  dplyr::select(-c(breeder:kaChx)) %>% # pull(season_yr) %>% unique()
  mutate(season = 
           case_when(
             season_yr == 2016 ~ 1617,
             season_yr == 2017 ~ 1718,
             season_yr == 2018 ~ 1819,
             season_yr == 2019 ~ 1920),
         crChx = 0,
         nuChx = 9,
         nuEgg = 8,
         kaChx = 0) 
  
# combine all historic nests
fishtagAll <-
  succFish %>% 
  rbind(notnTagGDR)

# general summary info
fishtagAll %>% 
  group_by(season) %>% 
  summarize(nNests = n(), kaChx = sum(kaChx), crChx = sum(crChx), totalChx = sum(nuChx)) 

# calculate transition mortality
fishtagComp <- 
  fishtagAll %>% 
  group_by(season) %>% 
  summarize(
    nNests = n(),
    BrSucc = mean(kaChx),
    Br.se = se(kaChx),
    CrSucc = mean(crChx),
    CR.se = se(crChx)) %>% 
  mutate(transMort = BrSucc - CrSucc)


# size threshold selection ------------------------------------------------

# How many chicks classed as GF survived to CR in our dataset?
# HOW MANY GF CHICKS MADE IT TO CR?

ftSizeCR <- 
  rawFish %>% 
  # find CR'd nests
  filter(grepl('CR', result)) %>%
  
  # convert CR1? case to CR1 confirmed after checking nest documentation
  mutate(result = if_else(result == 'CR1?',
                          'CR1',
                          result)) %>%
  # separate CR status from number of chx from each nest
  separate(result, 
           into = c("result", "number"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%  # pull(number) %>% is.na() %>% sum()
  
  # replace 2 NAs w. appropriate number
  mutate(number = as.numeric(number),
         correctNumb = if_else(is.na(number),
                               2,
                               number)) %>%
  # replace 8s with 1s after manual review
  mutate(correctNumb = if_else(correctNumb == 8,
                               1,
                               correctNumb)) %>% #pull(correctNumb)
  
  # add the failed nests back into the mix
  rbind(
    rawFish %>%  
      filter(!grepl('CR', result)) %>%
      mutate(number = 0, 
             correctNumb = 0)) %>% #group_by(season, location) %>% summarize(n = n())
  
  # add brood success (FinalCHsize >= GF) to the datatable
  mutate(FinalCHsize = 
           factor(FinalCHsize,
                  levels = c('UND', 'OR', 'GF', 'MEL', 'PA', 'PA+'),
                  ordered = T),
         BrSucc = if_else(
           FinalCHsize >= 'GF', # if we change this to GF criteria, BrSucc becomes larger than CrSucc (as we might expect)
           1,
           0))

# assign success to both chicks from CR2 nests
ftSizeCR %>% 
  filter(correctNumb == 2) %>% 
  transmute(season,
            bandnumb,
            result = 1,
            size = FinalCHsize) %>% 
  rbind(
    # assign success to largest chick from CR1 nests
    ftSizeCR %>% 
      filter(correctNumb == 1) %>% # group_by(season, bandnumb) %>% summarize(count = n()) %>% View()
      transmute(
        season, 
        bandnumb,
        chickNO,
        result = correctNumb,
        FinalCHsize) %>%
      group_by(season, bandnumb) %>% 
      mutate(
        trueResult = if_else(
          # the largest chick from each nest is assumed to creche
          FinalCHsize == max(FinalCHsize),
          1,
          0),
        trueResult = if_else(
          # if chicks same size, second chick marked fail
          (bandnumb %in% c(49389,51491,72023,46408) & str_detect(chickNO, pattern = '_2')),
          0,
          trueResult)) %>% 
      dplyr::select(season, bandnumb, result = trueResult, size = FinalCHsize)) %>% 
  rbind(
    ftSizeCR %>% 
      filter(correctNumb == 0) %>% 
      transmute(season,
                bandnumb,
                result = correctNumb,
                size = FinalCHsize)) %>% 
  group_by(size) %>% 
  summarize(propSucc = mean(result))
  
  
