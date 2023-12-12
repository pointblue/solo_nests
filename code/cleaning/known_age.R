## READING IN KNOWN AGE NEST SUCCESS FROM 2122

# setup -------------------------------------------------------------------


library(tidyverse)
library(data.table)
library(lubridate)



# KA nest outcomes -------------------------------------------------------------

initial_ka_obs <- read_csv("data/ka_resight21.csv")

ka_outcome <- read_csv("data/ka_nest_outcomes21.csv")

# filter out non-breeders
ka_outcome_br <- 
  ka_outcome %>%
  filter(result != "UNK") %>% # pull(result) %>% unique()
  # transform character outcome to numeric factor
  mutate(binaryKA = if_else(
    result == 'FAIL',
    0,
    1),
    # fill in blank subcolonies with areas
    subcolony = if_else(
      is.na(subcolony),
      location,
      subcolony)) %>% 
  # select relevant columns
  dplyr::select(
    c(nestid = nest,
      subcolony,
      binaryKA,
      result,
      result_dt,
      chick1size,
      chick2size)) 

# get nest location
ka_locs <- 
  initial_ka_obs %>%
  group_by(bandnumb) %>%
  summarise(latitude = min(lat), longitude = max(lon), .groups = 'drop') %>% 
  dplyr::rename(nestid = bandnumb)

# join the data
ka_rs <- 
  left_join(ka_outcome_br, ka_locs, by = c("nestid")) %>% # group_by(chick2size) %>% summarize(count = n())
  mutate(chick1size = case_when(
    chick1size == 'OR' ~ 'OR1',
    chick1size == 'MEL' ~ 'MEL1',
    TRUE ~ chick1size),
    chick2size = case_when(
      chick2size == 'OR'~'OR1',
      TRUE ~ chick2size)) %>% # group_by(chick2size) %>% summarize(count = n())
  mutate(type = 'ka',
         typeNum = 3,
         chick1size = factor(chick1size,
                             levels = c( 'LEM',
                                         'OR1',
                                         'OR2',
                                         'GF1',
                                         'GF2',
                                         'MEL1',
                                         'MEL2',
                                         'PA',
                                         'PA+'),
                             ordered = T),
         chick2size = factor(chick2size,
                             levels = c( 'LEM',
                                         'OR1',
                                         'OR2',
                                         'GF1',
                                         'GF2',
                                         'MEL1',
                                         'MEL2',
                                         'PA',
                                         'PA+'),
                                          ordered = T)) %>%
  # calculate number of chx creched
  mutate(kaChx = case_when(
    chick1size >= 'GF1' & chick2size >= 'GF1' ~ 2,
    chick1size >= 'GF1' | chick2size >= 'GF1' ~ 1,
    chick1size < 'GF1' ~ 0))


# KA chick age ------------------------------------------------------------
initial_ka_obs %>% 
  # find end of incubation
  filter(status == 'INC') %>% 
  # find only observations where contents directly observed
  filter(nuegg < 9 & nuegg > 0) %>% 
  dplyr::select(bandnumb, status, nuegg, lastInc = date) %>% 
  # convert date into an actual date
  mutate(lastInc = lubridate::mdy(lastInc)) %>% 
  # find maximum inc date for each nest
  group_by(bandnumb) %>% 
  filter(lastInc == max(lastInc)) %>% 
  # add maximum brood date
  left_join(
    (initial_ka_obs %>% 
      # find end of incubation
      filter(status == 'BR') %>% 
      # find only observations where contents directly observed
      filter(nuch < 9 & nuch > 0) %>% 
      # convert date into an actual date
      mutate(date = lubridate::mdy(date)) %>% 
      # find minimum brood date for each nest
      group_by(bandnumb) %>% 
      filter(date == min(date)) %>% 
      dplyr::select(bandnumb, brDate = date)),
    by = 'bandnumb') %>% 
  dplyr::select(bandnumb, lastInc, brDate) %>% 
  # 16 nests were never seen brooding
  filter(!is.na(brDate)) %>% 
  mutate(obsGap = as.numeric(brDate - lastInc)) %>%  pull(obsGap) %>% summary()



# KA chick size ----------------------------------------------------------------

ka_size_outcome <-
  tibble(ka_outcome_br) %>% 
  dplyr::select(c(nestid, result, date = result_dt, chick1size, chick2size)) %>%
  pivot_longer(cols = c(chick1size, chick2size),
               names_to = 'chick',
               values_to = 'size') %>% 
  filter(!is.na(size)) %>% 
  mutate(chick = if_else(
    chick == 'chick1size', 
    1,
    2),
    date = lubridate::mdy(date)) %>% 
  mutate(type = 'ka',
         size = case_when(
           size == 'OR' ~ 'OR1',
           size == 'MEL' ~ 'MEL1',
           TRUE ~ size),
         size = factor(size,
                       levels = c( 'LEM',
                                   'OR1',
                                   'OR2',
                                   'GF1',
                                   'GF2',
                                   'MEL1',
                                   'MEL2',
                                   'PA',
                                   'PA+'),
                       ordered = T)) #%>% 
  # select nests we believe succeeded
  # filter(size >= 'GF1') 

