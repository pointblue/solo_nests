# CORRECTED FISH-TAGGED NEST SUCCESS


# setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# fish-tag data -----------------------------------------------------------

# get fish-tagged nest success from 1617, 1718, 1819, and 1920
rawFish <- read_csv('data/FTCHXcombo.csv') 

# remove distant years
rawFish <-
  rawFish %>% filter(season != 1213 & season != 1314) %>% 
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
  
  # for each nest in each season, how many chx survived to CR?
  ## NOTE: Taking the mean but entries should be the same for each nest
  ### quality check for any non-integer values
  group_by(season, bandnumb) %>%
  summarise(crChx = mean(correctNumb), nuChx = Mode(nuch), nuEgg = mean(begin)) %>% # pull(crChx) %>% unique()
  
  # get rid of a very strange case (random ch likely stumbled into nest)
  filter(nuChx != 3) %>% 
  distinct() #%>% group_by(season) %>% summarize(seasonMean = mean(crChx))


 
fish_size_cr <-
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
                               correctNumb)) %>% 
  
  # select nests where both chicks made it or chicks which were physically recaped
  filter(correctNumb == 2 | banded == 1) %>% # group_by(season, bandnumb) %>% summarize(chx = mean(correctNumb)) %>% pull(chx) %>% sum()
  
  # add type (fish-tag)
  mutate(type = 1) %>% 
  dplyr::select(season, bandnumb, ch_bandnumb, FinalCHsize, type)


# ground count data ----------------------------------------------------------------

chxCount <- 
  read_csv('Z:/Informatics/S031/S0312122/Antarctica_EOS_report/data/croz_chick_count_compiled_v2022-04-20.csv')
adultCount <- 
  read_csv('Z:/Informatics/S031/S0312122/Antarctica_EOS_report/data/croz_adult_count_compiled_v2022-04-20.csv')

ground_count <-
  # pair active nests with number of chicks
  full_join(chxCount, adultCount, by = c('season', 'subcol')) %>%
  
  # select important variables
  select(colony = col.x, season, subCol = subcol, activeCt = active_ct, chCt = ch_ct) %>%
  
  # find years of interst
  filter(season >= 2016 & season != 2020) %>%
  
  # calculate average nest success in each subcolony
  mutate(avgSuccess = chCt/activeCt)

# merge subCol 21/24 in 2016 - 2017
ground_count <- 
  ground_count %>%
  filter(season == 2016 | season == 2017) %>%
  filter(subCol == "21" | subCol == '24' | subCol == '21/24' | subCol == '24-21') %>%
  mutate(subCol = '21/24') %>%
  group_by(season, subCol) %>%
  mutate(colony = 'croz', activeCt = sum(activeCt, na.rm = TRUE), chCt = sum(chCt, na.rm = TRUE), avgSuccess = (chCt / activeCt)) %>%
  distinct() %>%
  rbind(ground_count)

# find the average success within each year across all 30 sampled subcolonies
ground_count_avg <- 
  ground_count %>%
  filter(!is.na(chCt) & !is.na(activeCt)) %>%
  group_by(season) %>%
  summarize(activeCt = sum(activeCt),
            chCt = sum(chCt),
            avgSuccess = mean(avgSuccess)) #%>% filter(season != 2122) %>% summarize(m = mean(activeCt), s = sd(activeCt))

# change season values to match fish-tag data
 ground_count_avg$season[ground_count_avg$season == 2016] <- 1617
 ground_count_avg$season[ground_count_avg$season == 2017] <- 1718
 ground_count_avg$season[ground_count_avg$season == 2018] <- 1819
 ground_count_avg$season[ground_count_avg$season == 2019] <- 1920

 ground_count_avg$season[ground_count_avg$season == 2021] <- 2122
 
# clean -------------------------------------------------------------------

 # combine the cleaned data
fishtagComp <-
  succFish %>% 
  left_join(ground_count_avg, by = c('season')) %>%
   mutate(anomaly = crChx - avgSuccess, type = 'fish')
  
 rm(adultCount, chxCount, ground_count, rawFish, succFish)
  
