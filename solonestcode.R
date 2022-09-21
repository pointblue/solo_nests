## messing around with solo nest data

library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)

#setwd("Z:/Informatics/S031/S0312122/croz2122/solo_nests")


initial_solo_obs <- fread("solonest_initial_locations.csv")
# initial_solo_obs$date_created <- as_date(initial_solo_obs$date_created)
# write.csv(initial_solo_obs, "solonest_initial_locations.csv")

raw_solo_rs <- fread("solonest_obs_data_entry_2122.csv")
# change date format from Excel's dumb setting to Y-M-D
raw_solo_rs$date <- as_date(raw_solo_rs$date, 
                             format = "%m/%d/%Y")

marking_log <- fread("solonest_marked_chick_data_entry.csv")
marking_log$date <- as_date(marking_log$date, format = "%m/%d/%Y")


# left join of data, where df:raw_solo_rs contains duplicate keys
solo_rs <- raw_solo_rs %>%
  left_join(initial_solo_obs, by = "nestid") %>%
  filter(nestid != "solo27") # excludes solo27 (the one in G) due to SHEER INCONVENIENCE


# ---- LIST OF CURRENT NESTS  ----

## creating dfs for the various statuses to sort by

gone <- solo_rs %>%  
  group_by(nestid) %>%
  arrange(desc(date)) %>%
  filter(status.x == "GONE") %>%
  distinct(nestid)

failed <- solo_rs %>%
  group_by(nestid) %>%
  arrange(desc(date)) %>%
  filter(status.x == "FAIL") # solo_rs[status.x == "FAIL",]$nestid

occupied_nests <- solo_rs %>% # as in Currently Occupied, not as in Breeders
  filter(!nestid %in% gone$nestid 
         & !nestid %in% failed$nestid)

UNK_status <- solo_rs %>%
  group_by(nestid) %>%
  arrange(desc(date)) %>%
  distinct(nestid, .keep_all = T) %>%
  filter(status.x == "UNK" | status.x == "P/UNK")

cr_status_obs <- solo_rs %>%
  group_by(nestid) %>%
  arrange(desc(date)) %>%
  filter(status.x == "CR" | status.x == "CR?" 
         | status.x == "P/CR" | status.x == "P/CR?")

cr_confirmed <- cr_status_obs %>% # these can be dropped
  arrange(desc(date)) %>%
  distinct(nestid, .keep_all = T) %>%
  filter(status.x == "CR" | status.x == "P/CR")

crechetion <- cr_status_obs %>% # these still need to be checked
  group_by(nestid) %>%
  arrange(desc(date)) %>%
  filter(status.x == "CR?" |
           status.x == "CR?" & lag(status.x == "CR?"))

cr2x <- crechetion %>% # can drop if needed
  filter(status.x == "CR?" & lag(status.x == "CR?"))

# how many active nests are there?
inactive_nests <- occupied_nests %>%
  arrange(desc(date)) %>%             # these lines keep only the most recent 
  distinct(nestid, .keep_all = T) %>% # obs. for each nest
  filter(egg_n == 0 & chick_n == 0 # filters for no nest contents
         & !nestid %in% UNK_status$nestid # status UNK isn't considered inact
         & !nestid %in% cr_status_obs$nestid)  # status of CR or CR? is not inact either

active_nest_obs <- occupied_nests %>%
  filter(!nestid %in% inactive_nests$nestid
         & !nestid %in% cr_confirmed$nestid)

## NOTE: CR + CR? + FAIL + GONE = 44


# ---- LIST OF NESTS BY DATE LAST CHECKED & AREA ----

active_nests_by_date <- active_nest_obs %>%
  arrange(desc(date)) %>% # orders by date w/newest-seen at top
  distinct(nestid, .keep_all = TRUE) # keeps most recent 50 unique nestid val.s 

# CSV FOR ACTIVE NESTS -- NEED FOR MAPMAKING
filename <- paste("active_solos_", str_replace_all(Sys.Date(), "-", ""), ".csv", sep = "")
  # recall: Arc hates hyphens!

# write.csv(active_nests_by_date, filename)


int_end <- today()

# create intervals between last-check-date & today
check_int <- interval(start = active_nests_by_date$date, end = int_end)
# int_length(testdate) converts ints to seconds

# returns time since last checked in # of days
active_nests_by_date$time_since_checked <- seconds_to_period(check_int)

# places in new df for safety & arranges by time_since_checked with the 
# least recent checks at the top; also cuts out other data we don't need 
# to see for this purpose
checklist <- active_nests_by_date %>%
  arrange(desc(time_since_checked)) %>%
  select(nestid, status.x, egg_n, chick_n, ch1_size, ch2_size, area, date, time_since_checked) %>%
  rename(last_checked = date)

checklist$check_by_date <- as_date(today()) # filler w/Date format
class(checklist$check_by_date) # if this isn't Date something's wrong

# creating variables w/the different nest-check interval timings
no_egg_check_int <- checklist[egg_n == 0]$last_checked + days(4)
yes_egg_check_int <- checklist[egg_n > 0]$last_checked + days(7)
## if chick 0 and egg > 0, check 4 days anyway
## if chick > 0 and egg > 0, check 4 days anyway
yes_chick_check_int <- checklist[chick_n > 0]$last_checked + days(4)

checklist[egg_n == 0]$check_by_date <- no_egg_check_int
checklist[egg_n > 0]$check_by_date <- yes_egg_check_int
checklist[chick_n > 0]$check_by_date <- yes_chick_check_int

# arranging for more organized viewing, i.e. "nests to check ASAP" is at the top
#   ...or maybe I could just turn this into a function and write it so that I 
#   get back a real actual warning about this...
checklist <- checklist %>% 
  arrange(check_by_date) %>%
  filter(nestid != "solo39" # exclude cliff nest
         & nestid != "solo32" # exclude -- didn't mark, status UNK
         & nestid != "solo30" # exclude, suspect FAIL. this is the M/27 one
         & nestid != "solo44" # exclude, didn't mark so UNK for now
         & nestid != "solo43" # exclude, same as above
         & !nestid %in% cr2x$nestid) # excludes the ones that have been CR? 2+ times



# ---- HOW MANY MARKED CHICKS? ----

marking_log %>%
  group_by(nestid) %>%
  tally() %>%
  nrow() # returns number of nests w/marked chx - 21

marking_log %>%
  group_by(nestid) %>%
  arrange(desc(date)) %>%
  distinct(ch_id) %>%
  nrow() # returns number of marked chx - 27
    
creched <- active_nests_by_date %>%
  filter(!nestid %in% marking_log$nestid 
         & !nestid %in% creched$nestid # this line broken b/c decided to split creche lists up
         & !nestid == "solo39") # excluded b/c dangerous to mark

unmarked <- active_nests_by_date %>%
  filter(!nestid %in% marking_log$nestid 
         & !nestid %in% creched$nestid # this line broken b/c decided to split creche lists up
         & !nestid == "solo39") # excluded b/c dangerous to mark


# ---- LIGHTNING TALK: CR_confirmed v. FT CHX ----
FT_outcomes <- fread(file = "Z:/Informatics/S031/S0312122/croz2122/wb/fishtag_chick_outcomes.csv")
# wb_nests <- fread(file = "Z:/Informatics/S031/S0312122/croz2122/wb/wb_nests_2122.csv")
# wb_nests[outcome == 8]$outcome <- 1
# 
# wb_nests <- wb_nests %>%
#   distinct(wb_nests$nest_id, .keep_all = T)
#   
# wb_nests <- wb_nests %>% mutate(prod = wb_nests$outcome)
# mean(wb_nests$prod) # 1.18 -- this seems most correct

# how to get total # of breeding solos
breeder_rs <- solo_rs %>%
#  group_by(nestid) %>%
  filter(egg_n != 0 | chick_n != 0)
unique(breeder_rs$nestid) # 36 breeding nests?

FT_outcomes <- FT_outcomes %>% 
  distinct(FT_outcomes$nest_id, .keep_all = T)
mean(FT_outcomes$nestprod)

cr_confirmed$prod <- cr_confirmed$chick_n
cr_confirmed[prod == "8"]$prod

solo_outcomes <- fread("solo_outcomes.csv")
prod2 <- solo_outcomes[prod != "?" | prod != NA]$prod
mean(as.numeric(prod2))
length(solo_outcomes[breeder == 1]$breeder)


cr_solonests <- as.numeric(length(unique(cr_confirmed$nestid)))
n_solonests <- as.numeric(length(unique(breeder_rs$nestid)))

cr_FTnests <- 22
n_FTnests <- 28 # 28 total followed WB nests in 2122

chx_per_pair_solo <- cr_solonests/n_solonests #0.31
chx_per_pair_FT <- cr_FTnests/n_FTnests #0.76

# need to create DFs for each nest id so can calculate chx/pair for each pair
solo_cr_analysis <- solo_rs %>%
  filter(nestid %in% cr_confirmed$nestid)

 

df <- data.frame(
  who = c("Solo nests", "Weighbridge nests"),
  # creched = c(cr_solonests, cr_FTnests),
  # n_nests = c(n_solonests, n_FTnests),
  mean = c(chx_per_pair_solo, chx_per_pair_FT), 
  se = c(sd_solo/sqrt(n_solonests), sd_FT/sqrt(n_FTnests))
)

plot <- ggplot(data = df, aes(x = who, y = mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd)); plot





# marked_rs <- marking_log %>%
#   select(date, nestid, ch_id, symbol, poss_conflict) %>%
#   rename(mark_date = date) %>%
#   left_join(checklist, by = "nestid") %>%
#   select(!egg_n) %>%
#   relocate(mark_date, .before = ch_id) %>%
#   relocate(last_checked, .before = chick_n)
  









# count(initial_solo_obs[rocksize_cm == "0"])
# count(initial_solo_obs[pebble_distrib_nearby == "none"]); count(initial_solo_obs[pebble_distrib_nearby == "some"]); count(initial_solo_obs[pebble_distrib_nearby == "abundant"])
# 
# count(print_these_nests[rocksize_cm == "0"])
# count(print_these_nests[pebble_distrib_nearby == "none"])
# count(print_these_nests[pebble_distrib_nearby == "some"])
# count(print_these_nests[pebble_distrib_nearby == "abundant"])



# shp adventures ----
# obj: run something that creates a .shp with all current solo nests 
# & a .shp with all current Active nests.
# may also need to include something later for those that were marked 
# GONE & removed from the study?

# need to create a df from activenestdata (which is a list)
# so i can turn it into a SpatialPointsDF to go into writeOGR

names <- names(activenestdata)
activenestgps <- as.data.frame(matrix(NA, 
                                      length(names),
                                      ncol(activenestdata)))

activenestgps$nestid <- activenestdata$nestid


writeOGR(
  SpatialPointsDataFrame(
    coordinates(c(activenestdata$latitude, activenestdata$longitude))
  )
)


# messing around w/functions, ignore until i actually get shit working ----
gpsd_solos <- 
  function(status, # want "all", "active", "gone"
           filename = "") {
    if (status == "active") {
      activenestdata <- raw_solo_rs %>%
        filter(egg_n > 0)
      
      # use a semijoin to match activenestdata to initial_solo_obs? or 
      # maybe just go back to joined
        if (activenestdata$nestid = initial_solo_obs$nestid) {
          activegps$Latitude <- initial_solo_obs$Latitude
          activegps$Longitude <- initial_solo_obs$Longitude
        }
    }
  }

