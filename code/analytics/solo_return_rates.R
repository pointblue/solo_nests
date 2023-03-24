
# setup -------------------------------------------------------------------

library(tidyverse)

# load solo data from 2122
source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\solo_nest.R)')

solo_outcome <- read_csv("data/solo_outcomes.csv") %>% 
  separate(nestid, 
           into = c("type", "nestid"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  filter(!is.na(nestid)) %>%
  mutate(nestid = as.numeric(nestid)) %>% 
  dplyr::select(nestid,
                breeder = breeder)

solo_obs_2223 <- 
  read_csv('data/solonest_obs_data_entry_2223.csv') %>% 
  left_join(solo_outcome, by = 'nestid')

rm(solo_outcome)

# analyses ----------------------------------------------------------------

# 1. find solo nests which were active in 2223
active_solo_2223 <- 
  solo_obs_2223 %>% 
  # filter for birds that were seen incubating, brooding, or guarding
  filter(status == 'INC' |
           status == 'BR' |
           status == 'G') %>% 
  # and where at least one egg or chick was seen 
  filter(egg_n != 9 | chick_n != 9) %>% 
  mutate(date = lubridate::mdy(date),
         egg_n = if_else(is.na(egg_n),
                           0,
                           as.numeric(egg_n)),
         chick_n = if_else(is.na(chick_n),
                           0,
                           chick_n)) %>% 
  # summarize to breeding effort (1 vs. 2 eggs) and whether chicks seen
  arrange(nestid) %>% 
  group_by(nestid) %>% 
  summarize(
    effort = max(egg_n),
    chx = case_when(
      max(chick_n) > 0 ~ 1,
      max(chick_n) == 0 ~ 0),
    breeder2122 = max(breeder)) %>% 
  # identify which birds were successful last year
  left_join(
    as_tibble(solo_rs) %>%
      select(nestid = `...1`,
             crChx = confirmCR),
    by = 'nestid') %>% 
  mutate(lySuccess = 
           if_else(
             crChx > 0,
             1,
             0)) %>% 
  dplyr::select(-c(crChx))
  
# run a fisher exact test to see if successful breeding in 2122 influences liklihood of solo breed in 2223
solo_rs %>% 
  mutate(nestid = `...1`) %>% 
  select(c(nestid, outcome2122 = binaryCR)) %>% 
  full_join(
    active_solo_2223 %>% 
      dplyr::select(nestid, breeding2223 = effort)) %>% 
  mutate(breeding2223 = if_else(
           is.na(breeding2223),
         0,
         1)) %>% 
  group_by(outcome2122, breeding2223) %>% 
  summarize(count = n())
  

# now manually calculate the fisher's exact test statistic based on the formula described here: 
# https://en.wikipedia.org/wiki/Fisher%27s_exact_test

(factorial(15) * factorial(21) * factorial(11) * factorial(25)) / (factorial(6) * factorial(9) * factorial(16) * factorial(36))
