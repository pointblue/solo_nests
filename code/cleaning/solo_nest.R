

# source ------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)


# solo_nest_outcome ---------------------------------------------------------

# get outcomes for breeding solo nesters

solo_outcome <- read_csv("data/solo_outcomes.csv") 
solo_outcome_br <- solo_outcome %>% filter(!is.na(start)) 

# focus on important data

solo_outcome_br <- solo_outcome_br %>% dplyr::select(nestid, chN = chick_n, outcomeKA = outcome_ka, confirmCR = cr_confirm) 

# replace all values of 2 with 1 (breeders w. multiple chicks don't get extra credit)

# known-age criteria
succ <- solo_outcome_br %>% 
  filter(outcomeKA > 0) %>%
  mutate(outcomeKA = 1)

fail <- 
  solo_outcome_br %>% 
  filter(outcomeKA == 0)

solo_outcome_br <- rbind(succ, fail)
rm(succ, fail)

# confirmed criteria
solo_outcome_br <-
  solo_outcome_br %>% mutate(
  binaryCR = if_else(
    confirmCR == as.double(2),
    as.double(1),
    confirmCR))

# make outcome a factor variable

solo_outcome_br$outcomeKA <- factor(solo_outcome_br$outcomeKA, levels = c(0,1)) 

# solo_location -----------------------------------------------------------

# get initial locations

initial_solo_obs <- read_csv(
  "data/solonest_initial_locations.csv"
) 

# left join of data, where df:initial_solo_obs contains nest location and quality variables

solo_rs <- solo_outcome_br %>%
  left_join(initial_solo_obs, by = "nestid")

# get rid of extra columns

solo_rs <- solo_rs %>% dplyr::select(-c(northing, easting, description, egg_number, notes))


# habitat_variables -------------------------------------------------------

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

raw_solo <- read_csv("data/solonest_obs_data_entry_2122.csv")
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
            neighbs_yn = Mode(neighbor_y)
  )

solo_rs <- left_join(solo_rs, neighbs, by = ("nestid"))

# add additonal vars

solo_rs$position <- 0 # continuous variable for nest position

solo_rs$coarse_nestpos <- 0 # factor variable for nest position
solo_rs$coarse_nestpos <- factor(solo_rs$coarse_nestpos)

solo_rs$colonial <- 0 # factor variable for colonial or solo
solo_rs$colonial <- factor(solo_rs$colonial)

# exclude this bird because of large observation gap

#solo_rs <- solo_rs %>% filter(nestid != "solo44") 

rm(solo_outcome, solo_outcome_br, neighbs, raw_solo, initial_solo_obs) # cleanup

## SOLO SIZE ANALYSIS
knownCR <- 
  solo_rs %>% 
  filter(confirmCR >= 1) %>%
  pull(nestid)



solo_size_cr <- 
  read_csv("data/solonest_obs_data_entry_2122.csv") %>% 
  filter(nestid %in% knownCR) %>% 
  filter(status != 'CR' & status != 'CR?') %>% 
  filter(!is.na(ch1_size)) %>%
  mutate(date = lubridate::mdy(date)) %>% 
  group_by(nestid) %>% 
  filter(date == max(date)) %>% 
  summarize(size = ch1_size,
            flag = if_else(is.na(ch2_size), # flag cases when CR chick might be ambiguous
                           0,
                           1)) %>%
  rbind(filter(., nestid == 'solo6')) %>%
  mutate(season = 2122,
         type = 2)

solo_size_cr$size[solo_size_cr$nestid == 'solo26'] <- 'GF1'
solo_size_cr <- solo_size_cr %>% select(-c('flag'))

# flag was added based on the case of nest 26 where ch1 dies ch2 becomes ch1 btwn. checks
## as in the case of nest 26, where the smaller chick survived to CR

# Known_age_outcomes ------------------------------------------------------

# get data for breeding known age nesters

ka_outcome <- fread("data/ka_nest_outcomes21.csv")

# filter out non-breeders

ka_outcome_br <- ka_outcome %>% filter(result != "UNK")

# transform character outcome to numeric factor

succ <- ka_outcome_br %>% filter(result != "FAIL")
succ$outcomeKA <- 1
fail <- ka_outcome_br %>% filter(result == "FAIL")
fail$outcomeKA <- 0
ka_outcome_br <- rbind(succ, fail)
rm(succ, fail)

# focus on relevant data

ka_outcome_br <- ka_outcome_br %>% dplyr::select(c(nest, outcomeKA, result))


# Quality_check_ka_outcomes -----------------------------------------------

# Possible Errors:
# ** 1. are there any birds on there that disappeared below GF2?
# ** 2. Are there any birds above GF2 that didn't make the list?

# read in known age nest check data
initial_ka_obs <- read_csv("data/ka_resight21.csv")

# create vector of successful breeder bandnumbers

nests <- ka_outcome_br %>% filter(outcomeKA == 1)
nests <- c(nests$nest)

# find the last available size observation for each primary chick

initial_ka_obs$date <- mdy(initial_ka_obs$date)
final_ka_obs <- filter(initial_ka_obs,chick1size != "") %>%
  group_by(bandnumb) %>%
  filter(date == max(date)) 

# convert size to ordered factor
final_ka_obs$chick1size <- factor(
  final_ka_obs$chick1size,
  levels = c("LEM", "OR", "OR1", "OR2",
             "GF1", "GF2", "MEL", "MEL1",
             "MEL2", "PA", "PA+"),
  ordered = TRUE)

# Type 1 Error

smol <- final_ka_obs %>% filter(chick1size <= "GF1")
type1 <- smol[smol$bandnumb %in% nests,] # rejected null (fail) when null was true (failed)

# Type 2 Error

BIG <- final_ka_obs %>% filter(chick1size >= "GF2")
type2 <- BIG[!(BIG$bandnumb %in% nests),] # failed to reject null (fail) when null is false (success)

# neither type of error occurred
rm(final_ka_obs, smol, type1, BIG, type2)


# Known_age_location ------------------------------------------------------

# get most common nest position and location

initial_ka_obs <- initial_ka_obs %>%
  group_by(bandnumb) %>%
  summarise(position = Mode(nestpos),
            latitude = min(lat),
            longitude = max(lon))

initial_ka_obs <- initial_ka_obs %>% dplyr::rename(nest = bandnumb)

# join the data

ka_rs <- left_join(ka_outcome_br, initial_ka_obs, by = c("nest"))

# additional variables:

ka_rs$colonial <- 1 # identify as known-age, colony breeders

# filter for only nests that have position data

ka_rs <- ka_rs %>% filter(position != 0) 

# create factor variable for coarse position
center <- ka_rs %>% filter(position > 1)
center$coarse_nestpos <- 2
edge <- ka_rs %>% filter(position == 1)
edge$coarse_nestpos <- 1
ka_rs <- rbind(center, edge)
rm(center, edge)
ka_rs$coarse_nestpos <- factor(ka_rs$coarse_nestpos, levels = c(1,2))

rm(ka_outcome, ka_outcome_br, initial_ka_obs)


# naive_designations ------------------------------------------------------

nests <- c(seq(1:50))
nests <- paste("solo", nests, sep = "")
outcomes = c(NA, NA, 1, NA, 1, 1, NA, 1, NA, NA,
             NA, NA, 0, 1, 0, NA, 1, NA, NA, 1,
             1, 1, 1, 0, 1, 1, NA, 0, 1, 0,
             0, 1, 0, 1, 0, NA, 1, 1, 0, 0,
             0, 0, 0, NA, 1, NA, 1, 1, 0, 0)
a_designs <- data.frame(
  nestid = nests,
  outcome = outcomes
)
a_designs <- a_designs %>% filter(!is.na(outcome))
a_designs <- left_join(a_designs, solo_rs, by = c("nestid"))
a_designs <- a_designs %>% dplyr::select(c("nestid", "outcome", "outcomeKA"))

rm(nests)

