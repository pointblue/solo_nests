
# setup -------------------------------------------------------------------

source('code/analytics/summary_stats.R')
rm(resight_21)

solo_outcomes <- 
  read_csv('data/solo_outcomes.csv')

solo_locations <-
  read_csv('data/solonest_initial_locations.csv')

resight_solo <- read_csv("data/solonest_obs_data_entry_2122.csv") %>%
  mutate(date = lubridate::mdy(date))

r16 <-
  find_fishtag(resight_16, year = 1617)
r17 <-
  find_fishtag(resight_17, year = 1718)
r18 <-
  find_fishtag(resight_18, year = 1819)
r19 <-
  find_fishtag(resight_19, year = 1920)


# table 1 -----------------------------------------------------------------

# detailed table on nest characteristics
# possibly supplementary material

resight_solo %>%
  group_by(nestid) %>% 
  mutate(
    Duration = max(date) - min(date)) %>% 
  arrange(date) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  transmute(
    Year = 2021,
    Type = 'Solitary',
    nestid,
    Start = date,
    'Observation Period' = Duration) %>% 
  left_join(
    (solo_locations %>% 
       select(nestid,
              Lon = longitude, 
              Lat = latitude))) %>% 
  left_join(
    (solo_outcomes %>% 
      select(nestid,
             Outcome = cr_confirm)),
    by = 'nestid') %>% 
  filter(!is.na(Outcome)) %>% 
  rename(ID = 'nestid')

# general table describing characteristics between years
# year, type, number active nests, median hatch, median CR, avg sample success, avg. colony succ.

resight_all <- 
  resight_solo %>%
  mutate(season = 2122) %>%
  select(season,
         nestid,
         date,
         status) %>% 
  left_join(
    solo_outcomes %>% 
      select(nestid, Outcome = cr_confirm),
    by = 'nestid') %>% 
  left_join(
    ground_count_avg %>% 
      select(season, 'Colony Success' = avgSuccess),
    by = 'season'
  ) %>% 
rbind(
  (rbind(
    r16,
    r17,
    r18,
    r19) %>% 
     select(season,
            nestid = bandnumb,
            date,
            status) %>% 
     left_join((fishtagComp %>% 
                  select(season,
                         nestid = bandnumb,
                         Outcome = crChx,
                         'Colony Success' = avgSuccess)),
               by = c('season', 'nestid')) %>% 
     mutate(nestid = as.character(nestid))))

# find median hatch date
resight_all %>% 
  group_by(season, nestid) %>% 
  filter(grepl('BR', status)) %>% 
  filter(date == min(date) & nestid != 'solo27') %>% 
  distinct() %>%
  group_by(season) %>% 
  arrange(date) %>%
  summarize(
    'Median Hatch' = median(date))

# find median cr date
# see 'code/analytics/summary_stats.R for calculation of median CR dates

med_cr <- 
  tibble(
    season = c(1617,
               1718,
               1819,
               1920,
               2122),
    'Median CR' = c(
      "2017-01-05",
      "2018-01-15",
      "2019-01-04",
      "2020-01-05",
      "2022-01-10")) %>%
  mutate('Median CR' = lubridate::ymd(`Median CR`))

# find average within sample success
avg.succ <- 
  solo_comp %>%
  group_by(season) %>%
  summarize(min = min(crChx),
            q1 = quantile(crChx, 0.25),
            mean = mean(crChx),
            sd = sd(crChx),
            median = median(crChx),
            q3 = quantile(crChx, 0.75),
            max = max(crChx),
            n = n()) %>% 
  select(season,
         'Active nests' = n,
         'Average Sample Success' = mean,
         sd = sd)

# table1 <-
  resight_all %>%
    group_by(season, nestid) %>% 
    filter(grepl('BR', status)) %>% 
    filter(date == min(date) & nestid != 'solo27') %>% 
    distinct() %>%
    group_by(season) %>% 
    arrange(date) %>%
    summarize(
      'Median Hatch' = median(date),
      'Colony Success' = mean(`Colony Success`)) %>% 
    left_join(
      med_cr, by = 'season') %>% 
    left_join(avg.succ, by = 'season') %>% 
    mutate(
      Type = case_when(
        season == 1617 ~ 'Subcolony',
        season == 1718 ~ 'Subcolony',
        season == 1819 ~ 'Subcolony',
        season == 1920 ~ 'Subcolony',
        season == 2122 ~ 'Solitary')) %>% 
    select(season,
           Type,
           'Active nests',
           'Median Hatch',
           'Median CR',
           'Sample Success' = 'Average Sample Success',
           sd,
           'Colony Success')
    
  
  