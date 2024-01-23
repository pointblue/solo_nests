

# source ------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)

solo_outcome <- read_csv("data/solo_outcomes.csv") 

initial_solo_obs <- read_csv(
  "data/solonest_initial_locations.csv") 

raw_solo <- 
  read_csv("data/solonest_obs_data_entry_2122.csv") %>% 
  mutate(date = lubridate::mdy(date))


# compile solitary nest success metrics ---------------------------------------------------------

# get outcomes for breeding solo nesters

solo_outcome_br <-
  solo_outcome %>%
  filter(!is.na(start)) %>% 
  # focus on important data
  dplyr::select(nestid, chN = chick_n, confirmCR = cr_confirm)


# replace all values of 2 with 1 (breeders w. multiple chicks don't get extra credit)

# brood success criteria
brSucc_solo <- 
  raw_solo %>% # pull(status) %>% unique()
  # find all observations during brood or guard stage
  filter(grepl('BR', status) | grepl('G', status)) %>% 
  # remove solo27 because infrequently sampled / GONE observations
  filter(nestid != 'solo27' & status != 'GONE') %>%
  # filter out any observations without chick sizes since not useful for brood succ criteria
  filter(!is.na(ch1_size)) %>% 
  # standardize weird values
  mutate(ch1_size = case_when(
    ch1_size == 'OR' ~'OR1',
    ch1_size == 'MEL' ~ 'MEL1',
    ch1_size == 'PA++' ~ 'PA+',
    TRUE ~ ch1_size),
    ch2_size = case_when(
      ch2_size == 'OR1?' ~'OR1',
      ch2_size == 'MEL' ~ 'MEL1',
      TRUE ~ ch2_size)) %>% 
  # convert size to an ordered factor
  mutate(
    ch1_size = factor(ch1_size,
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
    ch2_size = factor(ch2_size,
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
  # find the latest observation for each nest
  group_by(nestid) %>%
  filter(date == max(date)) %>% 
  # select variables of interest
  dplyr::select(nestid, date, status, ch1_size, ch2_size) %>%
  # remove one duplicate observation which does not match size trajectory for nest
  filter(nestid != 'solo29' | ch1_size != 'GF1') %>% 
  ungroup() %>% 
  # calculate number of chx creched
  mutate(kaChx = case_when(
    ch1_size >= 'GF1' & ch2_size >= 'GF1' ~ 2,
    ch1_size >= 'GF1' | ch2_size >= 'GF1' ~ 1,
    ch1_size < 'GF1' ~ 0))

# compile success metrics
solo_success <-
  # creche success
  solo_outcome_br %>% mutate(
  binaryCR = if_else(
    confirmCR == as.double(2),
    as.double(1),
    confirmCR)) %>%
  # brood success
  left_join((brSucc_solo %>%
              dplyr::select(nestid, kaChx)),
            by = 'nestid')


# temporal summaries -------------------------------------------------
# days between inactive nest first observed and first empty
inactive_obs <- 
  raw_solo %>% 
  filter(!nestid %in% solo_outcome_br$nestid)

nest_occupancy_durration <- 
  inactive_obs %>% 
  dplyr::select(nestid, date, status, chick_n) %>% 
  # remove active but poorly observed nest
  filter(nestid != 'solo27') %>% 
  # group by nestid
  group_by(nestid) %>% 
  # find the first observation of each status (this is important to find the first day a nest was observed empty)
  group_by(nestid, status) %>% 
  filter(date == min(date)) %>% 
  # find difference between the first day nests were observed and the day a nest was empty
  group_by(nestid) %>%
  filter(date == min(date) | status == 'GONE') %>% 
  summarize(days_present = max(date) - min(date)) 

nest_occupancy_durration %>% 
  pull(days_present) %>% 
  summary()

# days between first chick seen and first chick disappearance
active_obs <- 
  raw_solo %>% 
  filter(nestid %in% solo_outcome_br$nestid)

# find single chick nests 
single_ch_nests <- 
  solo_outcome %>% 
  filter(chick_n == 2) #%>% nrow()

chicks_by_date <- 
  active_obs %>% 
  dplyr::select(nestid, date, status, chick_n) %>% 
  filter(grepl('BR', status) | grepl('G', status)) %>% 
  filter(nestid != 'solo27' & status != 'GONE') %>% 
  # remove any nests that only had 1 ch to start
  filter(!nestid %in% single_ch_nests$nestid) %>% # pull(nestid) %>% unique()
  # remove any unobserved chicks
  filter(chick_n != 9) %>% 
  # change at least one chick (8) to one chick (1)
  mutate(chick_n = if_else(chick_n == 8,
                           1, 
                           chick_n)) %>% 
  # select only the highest number of observations for each day
  group_by(nestid, date) %>% 
  filter(chick_n == max(chick_n)) %>% 
  # group by nestid
  group_by(nestid) %>% 
  # calculate a lagged difference in chick numbers
  mutate(
    ch_diff = chick_n - lag(chick_n)) %>% 
  # find the first day chicks were observed and the day a chick was lost
  filter(date == min(date) | ch_diff == -1) %>% 
  summarize(lost_ch = max(date) - min(date))
  

# add spatial and habitat data -----------------------------------------------------------

# get initial locations

# left join of data, where df:initial_solo_obs contains nest location and quality variables
solo_rs <- 
  solo_success %>%
  left_join(initial_solo_obs, by = "nestid") %>% 
  # get rid of extra columns
  dplyr::select(-c(northing, easting, description, egg_number, notes))



# factorize pebble distribution variable (0, 1, 2)

solo_rs$pebble_distrib_nearby <- factor(
  solo_rs$pebble_distrib_nearby,
  levels = c("none", "some", "abundant"))

solo_rs$pebble_distrib_nearby <- as.numeric(solo_rs$pebble_distrib_nearby)

solo_rs$pebble_distrib_nearby <- solo_rs$pebble_distrib_nearby - 1

solo_rs$pebble_distrib_nearby <- factor(
  solo_rs$pebble_distrib_nearby,
  levels = c(0,1,2))

# factorize rocksize_cm variable

solo_rs$rock_yn <- cut(
  solo_rs$rocksize_cm,
  breaks = c(-1,1,150),
  labels = c(0,1)
)

solo_rs$rock_yn <- factor(
  solo_rs$rock_yn,
  levels = c(0,1)
)

# create a variable for neighbor present

## new function to find the mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## read in nest check data

# examine NA entries
raw_solo %>% filter(is.na(n_neighbors)) # seems like NA == 0 in most cases
raw_solo$n_neighbors[is.na(raw_solo$n_neighbors)] <- 0

## factor variable for neighbors

yes <- raw_solo %>% filter(n_neighbors > 0)
yes$neighbor_y <- 1
no <- raw_solo %>% filter(n_neighbors == 0)
no$neighbor_y <- 0
raw_solo <- rbind(yes, no)
rm(yes,no)

# now find continuous neighbor variable

neighbs <- raw_solo %>% 
  group_by(nestid) %>% 
  summarize(prop_neighbs = mean(neighbor_y),
            neighbs_yn = Mode(neighbor_y),
            max_neighbs = max(n_neighbors))

solo_rs <- left_join(solo_rs, neighbs, by = ("nestid"))

# add additonal vars

solo_rs$position <- 0 # continuous variable for nest position

solo_rs$coarse_nestpos <- 0 # factor variable for nest position
solo_rs$coarse_nestpos <- factor(solo_rs$coarse_nestpos)

solo_rs$colonial <- 0 # factor variable for colonial or solo
solo_rs$colonial <- factor(solo_rs$colonial)

# exclude this bird because of large observation gap

#solo_rs <- solo_rs %>% filter(nestid != "solo44") 

rm(solo_outcome_br, neighbs, initial_solo_obs) # cleanup


# solo nest age -----------------------------------------------------------

raw_solo %>% 
  # find all observations during incubation
  filter(grepl('INC', status)) %>% # pull(status) %>% unique()
  # find only observations where contents directly observed
  filter(egg_n < 9 & egg_n > 0) %>% 
  dplyr::select(nestid, status, egg_n, lastInc = date) %>% 
  # find maximum inc date for each nest
  group_by(nestid) %>% 
  filter(lastInc == max(lastInc)) %>% 
  # remove solo27 and duplicates
  filter(nestid != 'solo27') %>% 
  filter(nestid != 'solo8' | egg_n != 8) %>% 
  # add maximum brood date
  left_join(
    (raw_solo %>% 
       # find end of incubation
       filter(grepl('BR', status)) %>% 
       # find only observations where contents directly observed
       filter(chick_n < 9 & chick_n > 0) %>% 
       # find minimum brood date for each nest
       group_by(nestid) %>% 
       filter(date == min(date)) %>% 
       dplyr::select(nestid, brDate = date)),
    by = 'nestid') %>% 
  dplyr::select(nestid, lastInc, brDate) %>% 
  # 16 nests were never seen brooding
  filter(!is.na(brDate)) %>% 
  mutate(obsGap = as.numeric(brDate - lastInc)) %>% pull(obsGap) %>% summary()

# solo size analysis  ----------------------------------------------------------

solo_size <-
  brSucc_solo %>% 
  dplyr::select(nestid, date, ch1_size, ch2_size) %>% 
  pivot_longer(cols = c(ch1_size, ch2_size), names_to = 'chick', values_to = 'size') %>% 
  # manually designate which chicks were resighted in CR
  mutate(
    chick = if_else(
      chick == 'ch1_size',
      1,
      2),
    creche = 
           case_when(
             nestid == 'solo14' & size == 'MEL2' ~ 1,
             nestid == 'solo17' & size == 'PA+' ~ 1,
             nestid == 'solo20' & size == 'PA+' ~ 1,
             nestid == 'solo22' & size == 'PA+' ~ 1,
             nestid == 'solo23' & size == 'PA' ~ 1,
             nestid == 'solo26' & size == 'GF1' ~ 1,
             nestid == 'solo3' & size == 'MEL2' ~ 1,
             nestid == 'solo34' & size == 'PA+' ~ 1,
             nestid == 'solo5' & size == 'MEL1' ~ 1,
             nestid == 'solo8' & size == 'PA+' ~ 1,
             nestid == 'solo6' ~ 1,
             TRUE ~ 0
           ))

# the case of nest 26 where ch1 dies ch2 becomes ch1 btwn. checks
## as in the case of nest 26, where the smaller chick survived to CR

