
# setup -------------------------------------------------------------------

library(tidyverse)

# load solo data from 2122
source('code/cleaning/solo_nest.R')

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

# explore -----------------------------------------------------------------

# find cases of ambiguous nest identity
solo_obs_2223 %>% 
  pull(status) %>% 
  unique()

# filter out unambiguous obs.

solo_obs_2223 %>% 
  # ambiguous cases are unlikely to be labeled as INC BR OR G
  filter(!status %in% c('INC', 'BR', 'G', 'P/INC', 'P/WBN', 'WBN')) %>% #View()
  # ambiguous cases unlikely to be clearly MT or 'NBS
  filter(!status %in% c('MT', 'NBS')) # %>% View()


# format ----------------------------------------------------------------

## active solo nests -------------------------------------------------------

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
      transmute(nestid = `...1`,
                    crChx = confirmCR,
                    kaChx = if_else(
                      is.na(kaChx),
                      0,
                      kaChx)),
    by = 'nestid') %>% 
  mutate(CrSuccess = 
           if_else(
             crChx > 0,
             1,
             0),
         BrSuccess = 
           if_else(
             kaChx > 0,
             1,
             0)) %>% 
  dplyr::select(-c(crChx, kaChx))


## occupied solo nests -----------------------------------------------------

# 1.5: generate a list of those that were not active
occupied_solo_2223 <- 
  solo_obs_2223 %>%
  filter(!nestid %in% active_solo_2223$nestid) %>% 
  # remove all nests where nobody was seen
  filter(!is.na(status) & !status %in% c('MT', 'UNK', 'INC?', 'NBS', 'FAIL?')) %>% 
  transmute(
    nestid = nestid,
    effort = 9,
    chx = 0) %>% 
  left_join(
    as_tibble(solo_rs) %>%
      transmute(nestid = `...1`,
                crChx = confirmCR,
                kaChx = if_else(
                  is.na(kaChx),
                  0,
                  kaChx)),
    by = 'nestid') %>% 
  mutate(CrSuccess = 
           if_else(
             crChx > 0,
             1,
             0),
         BrSuccess = 
           if_else(
             kaChx > 0,
             1,
             0),
         active2223 = 0) %>% 
  dplyr::select(-c(crChx, kaChx)) %>%
  distinct() %>%
  left_join(solo_outcome, by = 'nestid') %>% 
  rename(breeder2122 = breeder) %>% 
  rbind((active_solo_2223 %>% 
          mutate(active2223 = 1)))


# compiled ----------------------------------------------------------------

solo_resight_22 <-
  tibble(
    nestid = c(1,3:6,8:50)) %>% 
  mutate(
    reOcc = if_else(
      nestid %in% (occupied_solo_2223 %>% pull(nestid)),
      1,
      0),
    reAct = if_else(
      nestid %in% (active_solo_2223 %>% pull(nestid)),
      1,
      0),
    ambig = if_else(
      nestid %in% c(4, 13, 19, 42, 9, 10),
      1,
      0)) %>% 
  # remove nest with unknown outcome
  filter(nestid != 27)


# analyses ----------------------------------------------------------------

# now manually calculate the fisher's exact test statistic based on the formula described here: 
# https://en.wikipedia.org/wiki/Fisher%27s_exact_test

## success > occup. -----------------------------------------------------
# were active nests more likely to be re-occupied?
solo_rs %>% 
  mutate(nestid = `...1`) %>% 
  dplyr::select(c(nestid, CrSucc2122 = confirmCR, BrSucc2122 = kaChx)) %>% 
  # bind backwards to include nests which were not active in 2022
  full_join(
    solo_resight_22 %>% 
      dplyr::select(nestid, occupied2223 = reOcc)) %>% 
  # success of non-breeders to zero success
  mutate(CrSucc2122 = case_when(
    is.na(CrSucc2122) ~ 0,
    CrSucc2122 == 0 ~ 0,
    CrSucc2122 >= 1 ~ 1),
    BrSucc2122 = case_when(
      is.na(BrSucc2122) ~ 0,
      BrSucc2122 == 0 ~ 0,
      TRUE ~ 1)) %>%  
  # remove ambiguous nests
  filter(!nestid %in% 
           (solo_resight_22 %>%
           filter(ambig == 1) %>%
           pull(nestid))) %>%
  # choose success metric and count outcomes in each category
  group_by(BrSucc2122, occupied2223) %>% 
  summarize(count = n())

# creche success
(factorial(20) * factorial(21) * factorial(11) * factorial(30)) / (factorial(4) * factorial(7) * factorial(13) * factorial(17) * factorial(41))
# p = 0.13
# brood success
(factorial(20) * factorial(21) * factorial(18) * factorial(23)) / (factorial(14) * factorial(4) * factorial(7) * factorial(16) * factorial(41))
# p = 0.002

## success > active -----------------------------------------------------
# does successful breeding in 2122 influences liklihood of solo breed in 2223
solo_rs %>% 
  mutate(nestid = `...1`) %>% 
  dplyr::select(c(nestid, CrSucc2122 = confirmCR, BrSucc2122 = kaChx)) %>% 
  # bind backwards to include nests which were not active in 2022
  full_join(
    solo_resight_22 %>% 
      dplyr::select(nestid, breeding2223 = reAct)) %>% 
  # success of non-breeders to zero success
  mutate(CrSucc2122 = case_when(
    is.na(CrSucc2122) ~ 0,
    CrSucc2122 == 0 ~ 0,
    CrSucc2122 >= 1 ~ 1),
    BrSucc2122 = case_when(
      is.na(BrSucc2122) ~ 0,
      BrSucc2122 == 0 ~ 0,
      TRUE ~ 1)) %>%  
  # remove ambiguous nests
  filter(!nestid %in% 
           (solo_resight_22 %>%
              filter(ambig == 1) %>%
              pull(nestid))) %>%
  # choose success metric and count outcomes in each category
  group_by(BrSucc2122, breeding2223) %>% 
  summarize(count = n())
  
# creching success
(factorial(15) * factorial(26) * factorial(11) * factorial(30)) / (factorial(5) * factorial(6) * factorial(9) * factorial(21) * factorial(41))
# p = 0.104
# brood success
(factorial(15) * factorial(26) * factorial(23) * factorial(18)) / (factorial(12) * factorial(11) * factorial(3) * factorial(15) * factorial(41))
# p = 0.00987

## active > occup. ---------------------------------------------------------
solo_rs %>% 
  transmute(nestid = `...1`, active2122 = 1) %>% 
  # bind backwards to include nests which were not active in 2022
  full_join(
    solo_resight_22 %>% 
      dplyr::select(nestid, occupied2223 = reOcc)) %>% 
  # success of non-breeders to zero success
  mutate(active2122 = if_else(
    is.na(active2122),
    0,
    active2122)) %>%  
  # choose success metric and count outcomes in each category
  # remove ambiguous nests
  filter(!nestid %in% 
           (solo_resight_22 %>%
              filter(ambig == 1) %>%
              pull(nestid))) %>%
  group_by(active2122, occupied2223) %>% 
  summarize(count = n())

# active 2122
(factorial(20) * factorial(22) * factorial(34) * factorial(8)) / (factorial(20) * factorial(14) * factorial(8) * factorial(0) * factorial(42))
# p = 0.002

## active > active ---------------------------------------------------------
solo_rs %>% 
  transmute(nestid = `...1`, active2122 = 1) %>% 
  # bind backwards to include nests which were not active in 2022
  full_join(
    solo_resight_22 %>% 
      dplyr::select(nestid, breed2223 = reAct)) %>% 
  # success of non-breeders to zero success
  mutate(active2122 = if_else(
    is.na(active2122),
    0,
    active2122)) %>%  
  # choose success metric and count outcomes in each category
  # remove ambiguous nests
  filter(!nestid %in% 
           (solo_resight_22 %>%
              filter(ambig == 1) %>%
              pull(nestid))) %>%
  group_by(active2122, breed2223) %>% 
  summarize(count = n())

# active 2122
(factorial(15) * factorial(34) * factorial(27) * factorial(8)) / (factorial(15) * factorial(19) * factorial(0) * factorial(8) * factorial(42))
# p = 0.018




## ambiguous nests ---------------------------------------------------------

### success > occup. ---------------------------------------------------------
solo_rs %>% 
  mutate(nestid = `...1`) %>% 
  dplyr::select(c(nestid, CrSucc2122 = confirmCR, BrSucc2122 = kaChx)) %>% 
  # bind backwards to include nests which were not active in 2022
  full_join(
    solo_resight_22 %>%
      mutate(reOcc = if_else(
        ambig == 1,
        1,
        reOcc)) %>% 
      dplyr::select(nestid, occup.2223 = reOcc)) %>% 
  # success of non-breeders to zero success
  mutate(CrSucc2122 = case_when(
    is.na(CrSucc2122) ~ 0,
    CrSucc2122 == 0 ~ 0,
    CrSucc2122 >= 1 ~ 1),
    BrSucc2122 = case_when(
      is.na(BrSucc2122) ~ 0,
      BrSucc2122 == 0 ~ 0,
      TRUE ~ 1)) %>%  
  # choose success metric and count outcomes in each category
  group_by(CrSucc2122, occup.2223) %>% 
  summarize(count = n())

# confirmed success
(factorial(26) * factorial(21) * factorial(11) * factorial(36)) / (factorial(19) * factorial(17) * factorial(7) * factorial(4) * factorial(47))
# p = 0.226
# assumed success
(factorial(26) * factorial(21) * factorial(23) * factorial(24)) / (factorial(14) * factorial(10) * factorial(7) * factorial(16) * factorial(47))
# p = 0.0383

