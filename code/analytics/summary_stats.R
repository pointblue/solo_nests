
# setup -------------------------------------------------------------------

library(tidyverse)

source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\fish-tags.R)')

resight_16 <- read_csv(r'(Z:\Informatics\S031\S0311617\croz1617\bandsearch\resight16.csv)') %>%
  mutate(date = lubridate::mdy(date),
         season = 1617)
resight_17 <- read_csv(r'(Z:\Informatics\S031\S0311718\croz1718\bandsearch\resight17.csv)')%>%
  mutate(date = lubridate::mdy(date),
         season = 1718)
resight_18 <- read_csv(r'(Z:\Informatics\S031\S0311819\croz1819\bandsearch\resight18.csv)')%>%
  mutate(date = lubridate::mdy(date), 
         season = 1819)
resight_19 <- read_csv(r'(Z:\Informatics\S031\S0311920\croz1920\bandsearch\resight19.csv)') %>%
  mutate(date = lubridate::mdy(date), 
         season = 1920)
resight_21 <- read_csv(r'(Z:\Informatics\S031\S0312122\croz2122\bandsearch\resight21.csv)') %>% 
  mutate(date = lubridate::mdy(date), 
         season = 2122)


# non-breeders ------------------------------------------------------------

prop.nonbreed <-
  function(resight) {
    resight %>%
      group_by(bandnumb) %>% 
      filter
  }

resight_21 %>% 
  pull(bandnumb) %>% 
  unique() %>% 
  length()

resight_21 %>% 
  group_by(bandnumb) %>% 
  filter(max(nuegg) == 0 | n() == 1) %>%
  pull(bandnumb) %>% 
  unique() %>% 
  length()

solo_comp %>% group_by(typeNum, season) %>% summarize(n = n(), chx = sum(crChx), prop = chx/n)


# egg / chx appearance ----------------------------------------------------

# median incubation date solitary nests
raw_solo <- read_csv("data/solonest_obs_data_entry_2122.csv") %>%
  mutate(date = lubridate::mdy(date))

raw_solo %>%
  group_by(nestid) %>% 
  filter(grepl('INC', status)) %>% filter(date == min(date) & nestid != 'solo27') %>% 
  distinct() %>%
  arrange(date) %>% 
  pull(date) %>% 
  median()
  
# median incubation date subcolony nests
rawFish <- read_csv('data/FTCHXcombo.csv')

resight_16 %>% 
  filter(bandnumb %in% (rawFish %>%
           filter(season == 1617) %>% pull(bandnumb))) %>% 
  group_by(bandnumb) %>% 
  filter(grepl('INC', status)) %>% filter(date == min(date) & bandnumb != 50416) %>% 
  distinct() %>% group_by(bandnumb) %>% summarize(date = min(date)) %>% #summarize(l = length(unique(date))) %>% View()
  pull(date) %>% 
  median()

resight_17 %>% 
  filter(bandnumb %in% (rawFish %>%
                          filter(season == 1718) %>% pull(bandnumb))) %>% 
  group_by(bandnumb) %>% 
  filter(grepl('INC', status)) %>% filter(date == min(date)) %>% 
  distinct() %>% group_by(bandnumb) %>% summarize(date = min(date)) %>% #summarize(l = length(unique(date))) %>% View()
  pull(date) %>% 
  median()
  
resight_18 %>% 
  filter(bandnumb %in% (rawFish %>%
                          filter(season == 1819) %>% pull(bandnumb))) %>% 
  group_by(bandnumb) %>% 
  filter(grepl('INC', status)) %>% filter(date == min(date)) %>% 
  distinct() %>% group_by(bandnumb) %>% summarize(date = min(date)) %>% #summarize(l = length(unique(date))) %>% View()
  pull(date) %>% 
  median()

resight_19 %>% 
  filter(bandnumb %in% (rawFish %>%
                          filter(season == 1920) %>% pull(bandnumb))) %>% 
  group_by(bandnumb) %>% 
  filter(grepl('INC', status)) %>% filter(date == min(date)) %>% 
  distinct() %>% group_by(bandnumb) %>% summarize(date = min(date)) %>% #summarize(l = length(unique(date))) %>% View()
  pull(date) %>% 
  median()

inc <- c("2016-11-15", "2017-11-14", "2018-11-12", "2019-11-16") %>% lubridate::yday()
(inc - lubridate::yday("2021-11-26")) %>% mean()
# MEDIAN HATCH DATE
# median hatch date solitary
raw_solo %>%
  group_by(nestid) %>% 
  filter(grepl('BR', status)) %>% filter(date == min(date) & nestid != 'solo27') %>% 
  distinct() %>%
  pull(date) %>% 
  median()

# median hatch date subcolony nests
resight_16 %>% 
  filter(bandnumb %in% (rawFish %>%
                          filter(season == 1617) %>% pull(bandnumb))) %>% 
  group_by(bandnumb) %>% 
  filter(grepl('BR', status)) %>% filter(date == min(date) & bandnumb != 50416) %>% 
  distinct() %>% group_by(bandnumb) %>% summarize(date = min(date)) %>% #summarize(l = length(unique(date))) %>% View()
  pull(date) %>% 
  median()

resight_17 %>% 
  filter(bandnumb %in% (rawFish %>%
                          filter(season == 1718) %>% pull(bandnumb))) %>% 
  group_by(bandnumb) %>% 
  filter(grepl('BR', status)) %>% filter(date == min(date)) %>% 
  distinct() %>% group_by(bandnumb) %>% summarize(date = min(date)) %>% #summarize(l = length(unique(date))) %>% View()
  pull(date) %>% 
  median()

resight_18 %>% 
  filter(bandnumb %in% (rawFish %>%
                          filter(season == 1819) %>% pull(bandnumb))) %>% 
  group_by(bandnumb) %>% 
  filter(grepl('BR', status)) %>% filter(date == min(date)) %>% 
  distinct() %>% group_by(bandnumb) %>% summarize(date = min(date)) %>% #summarize(l = length(unique(date))) %>% View()
  pull(date) %>% 
  median()

resight_19 %>% 
  filter(bandnumb %in% (rawFish %>%
                          filter(season == 1920) %>% pull(bandnumb))) %>% 
  group_by(bandnumb) %>% 
  filter(grepl('BR', status)) %>% filter(date == min(date)) %>% 
  distinct() %>% group_by(bandnumb) %>% summarize(date = min(date)) %>% #summarize(l = length(unique(date))) %>% View()
  pull(date) %>% 
  median()

br <- 
  c("2016-12-20", "2017-12-19", "2018-12-17", "2019-12-23") %>% 
  lubridate::yday()

(br - lubridate::yday("2021-12-23")) %>% mean()

# MEDIAN CR DATE
# median cr solitary
raw_solo %>%
  group_by(nestid) %>% 
  filter(nestid %in% (solo_rs %>% filter(confirmCR >= 1) %>% pull(nestid))) %>% 
  filter(grepl('CR', status)) %>% filter(date == min(date) & nestid != 'solo27') %>% 
  distinct() %>%
  pull(date) %>% 
  median()

# median cr subcolony

  rawFish %>% 
    filter(season==1617 & bandnumb %in% (fishtagComp %>% filter(season == 1617 & crChx >= 1) %>% pull(bandnumb))) %>%
    filter(bandnumb != 50416) %>% 
    transmute(season,
              bandnumb,
              result_dt = lubridate::mdy(result_dt)) %>% 
    distinct() %>%
    pull(result_dt) %>% 
    median()
    
  rawFish %>% 
    filter(season==1718 & bandnumb %in% (fishtagComp %>% filter(season == 1718 & crChx >= 1) %>% pull(bandnumb))) %>%
    transmute(season,
              bandnumb,
              result_dt = lubridate::mdy(result_dt)) %>% 
    distinct() %>%
    pull(result_dt) %>% 
    median()
  
  rawFish %>% 
    filter(season==1819 & bandnumb %in% (fishtagComp %>% filter(season == 1819 & crChx >= 1) %>% pull(bandnumb))) %>%
    transmute(season,
              bandnumb,
              result_dt = lubridate::mdy(result_dt)) %>% 
    distinct() %>%
    pull(result_dt) %>% 
    median()
  
  rawFish %>% 
    filter(season==1920 & bandnumb %in% (fishtagComp %>% filter(season == 1920 & crChx >= 1) %>% pull(bandnumb))) %>%
    transmute(season,
              bandnumb,
              result_dt = lubridate::mdy(result_dt)) %>% 
    distinct() %>%
    pull(result_dt) %>% 
    median()
  
  cr <-
    c("2017-01-05",
      "2018-01-15",
      "2019-01-04",
      "2020-01-05") %>% 
    lubridate::yday()
  
  (cr - lubridate::yday("2022-01-10")) %>% abs() %>% mean()


# avg. observation gap ----------------------------------------------------

raw_solo %>%
    filter(nestid %in% (solo_rs %>% pull(nestid))) %>% 
    group_by(nestid) %>% 
    summarize(deltaObs = date - lag(date)) %>% 
    summarize(m = mean(deltaObs, na.rm = TRUE)) %>% 
    pull(m) %>% 
    mean()
  
resight_16 %>% 
  rbind(resight_17, resight_18, resight_19) %>% 
  filter(bandnumb %in% (fishtagComp %>% pull(bandnumb))) %>%
  group_by(season, bandnumb) %>% 
  summarize(deltaObs = date - lag(date)) %>% 
  summarize(m = mean(deltaObs, na.rm = TRUE)) %>% 
  pull(m) %>% 
  mean(na.rm = TRUE)


# distance to subcolony ---------------------------------------------------

nest_distance <- read_csv(
  "data/solonest_initial_locations.csv") %>% 
  filter(nestid != 'solo27') %>% 
  pull(dist_nearest_subcol_nest_m) %>% 
  mean()

# skua notes --------------------------------------------------------------

raw_solo %>% 
  filter(grepl('skua', notes) | grepl('SPSK', notes) | grepl('Skua', notes) | grepl('SKUA', notes)) 

# number of eggs / chx -------------------------------------------------------------------

# solitary nests
solo_outcome <- read_csv("data/solo_outcomes.csv") %>%
  filter(nestid != 'solo27')

solo_outcome %>%
  filter(!is.na(egg_n)) %>% 
  mutate(egg_n = if_else(egg_n == 8,
                         1,
                         egg_n)) %>% summarize(m = mean(egg_n), sd = sd(egg_n))
  group_by(egg_n) %>% 
  summarize(n = n()) %>%
  mutate(prop = n/sum(n))

solo_outcome %>%
  filter(!is.na(chick_n)) %>% 
  mutate(egg_n = if_else(egg_n == 8,
                         1,
                         egg_n),
    chick_n = if_else(chick_n == 8,
                         1,
                         chick_n)) %>% 
  group_by(egg_n, chick_n) %>% 
  summarize(n = n()) %>%
  mutate(prop = n/sum(n))

# subcolony nests
fishtagComp %>% summarize(m = mean(nuEgg), sd = sd(nuEgg))
  group_by(nuEgg) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))

fishtagComp %>%
  mutate(nuChx = if_else(nuChx == 8,
                           1,
                           nuChx)) %>%
  group_by(nuEgg, nuChx) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))


# size distribution -------------------------------------------------------

fish_size_cr %>%
  transmute(nestid = as.character(bandnumb),
            size = FinalCHsize,
            ch_bandnumb,
            season,
            type) %>%
  rbind(solo_size_cr %>% 
          mutate(ch_bandnumb = 1)) %>% # pull(size) %>% unique()
  filter(size != 'UND') %>% 
  mutate(season = case_when(
    season == 1617 ~ 2016,
    season == 1718 ~ 2017,
    season == 1819 ~ 2018,
    season == 1920 ~ 2019,
    season == 2122 ~ 2021
  ),
  size = factor(size, levels = c('OR', 'GF', 'GF1', 'GF2', 'MEL', 'MEL1', 'MEL2', 'PA', 'PA+', 'PA++'))) %>% 
  mutate(z = cut(as.numeric(size), breaks = c(0,1,4,7,9), labels = c('A', 'B', 'C', 'D')),
         type = if_else(type == 1, 'Subcolony', 'Solitary')) %>% # select(size,z) %>% View()
  group_by(season,z) %>% 
  summarize(n = n()) %>%
  pivot_wider(names_from = z, values_from = n) %>%
  mutate(A = if_else(is.na(A),
                     as.integer(0),
                     A)) %>% 
  rowwise() %>% 
  mutate(total = sum(A,B,C,D),
         A = A/total,
         B = B/total,
         C = C/total,
         D = D/total) %>%
  ungroup() %>% 
  pivot_longer(!c(season,total),
               names_to = 'Size',
               values_to = 'prop') %>% filter(season != 2021) %>% 
  group_by(Size) %>% 
  summarize(meanProp = mean(prop), sdProp = sd(prop))
