
# setup -------------------------------------------------------------------

library(tidyverse)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

raw_obs <- read_csv(
  "data/solonest_obs_data_entry_2122.csv"
) 


# hatching success --------------------------------------------------------

raw_obs <- 
  raw_obs %>% 
  # fix formatting issues
  separate(nestid, into = c("text", "nestid"), sep = "(?<=[A-Za-z])(?=[0-9])")


# function ----------------------------------------------------------------

hatch_success <-
  function(data, bandnumb = 'bandnumb', # name of column with band number data
           eggs = 'nuegg', # name of column w. number of egg obs. 
           chicks = 'nuch', # name of column w. number chx obs. 
           status = 'status' # name of column w. nest status obs. 
           ) {
  
  library(tidyverse)
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  raw_obs <- data
  raw_obs <- 
    raw_obs %>% rename(nestid = bandnumb,
                       egg_n = eggs,
                       chick_n = chicks,
                       status = status)
  
  if(class(raw_obs$nestid) != 'numeric') {
    warning('Band number is non-numeric, trying to convert...', call. = FALSE)
    raw_obs$nestid <- as.numeric(raw_obs$nestid)
  }
  
  # 1. Find breeding birds only
  breed <- 
    raw_obs %>% 
    # fix formatting issues
    mutate(date = lubridate::mdy(date)) %>% 
    group_by(nestid) %>% 
    # 1. find breeding birds
    filter(max(egg_n) != 0)
  
  # 2. Normalize the data
  
  Incubation <- 
    # select data from the incubation stage
    breed %>%
    filter(grepl('INC', status)) %>% 
    dplyr::select(c(nestid, date, status, egg_n, notes))
  
  Brood <-
    # select nest data from brooding stage
    breed %>% 
    filter(grepl('BR', status)) %>% 
    dplyr::select(c(nestid, date, status, egg_n, chick_n, notes))
  
  # 3 find problematic penguins (penguins which had mostly unobserved states)
  # 3.1: Incubating penguins
  unobsINC <- 
    Incubation %>%
    group_by(nestid) %>% 
    # find most common observation state for each nest
    summarise(eggMode = Mode(egg_n)) %>%
    # filter for birds most often unobserved 
    filter(eggMode >= 8)
  
  # 3.2: Brooding penguins 
  unobsBR <-
    Brood %>%
    group_by(nestid) %>%
    # find most common observation state for each nest
    summarize(chxMode = Mode(chick_n)) %>%
    # filter for birds most often unobserved
    filter(chxMode >= 8) 
  
  # 3.2.1: Remove flags w. 2 observed chx
  unobsINC <- 
    unobsINC %>%
    filter(!nestid %in% (Brood %>% 
                           filter(chick_n < 8) %>%
                           # find nests w. maximal success
                           filter(max(chick_n) == 2) %>%
                           # select max nests which are also in unobserved INC
                           filter(nestid %in% unobsINC$nestid[!unobsINC$nestid  %in% unobsBR$nestid]) %>%
                           pull(nestid)))

  
  # 3.3: Initially unobserved Brooding penguins (observation gap) w. < 2 chx
  firstBR <- Brood %>% 
    mutate(z = if_else(chick_n >= 8, -8, chick_n)) %>% 
    filter(max(z) != 2) %>% 
    arrange(nestid, date) %>%
    filter(date == min(date))
  
  ## add nests which were initially unobserved to flag list
  unobsBR <-
    firstBR %>%
    filter(chick_n >= 8) %>%
    select(nestid, chick_n) %>%
    full_join(unobsBR, by = 'nestid')
  
  # 3.4: find the time gap between the last INC and first BR observations
  observation_gap <- Incubation %>%  
    arrange(nestid, date) %>% 
    group_by(nestid) %>% 
    filter(date == max(date)) %>% 
    select(nestid, date) %>%
    left_join(Brood %>%  
                arrange(nestid, date) %>% 
                group_by(nestid) %>% 
                filter(date == min(date)) %>% 
                select(nestid, date), by = 'nestid') %>%
    mutate(obsGap = (date.y - date.x)) %>%
    select(nestid, obsGap)
  
  # NOTE: THIS SECTION OF THE CODE COULD BE EXPANDED TO BUILD A SMARTER FUNCTION:
  ## flag only birds with large gaps between first and subsequent few BR observs.  
  
  # 3.5: print an informative warning for the user
  flagged <-
    c(unobsBR$nestid, unobsINC$nestid) %>%
    .[!duplicated(.)] %>% 
    sort()
  
  if(length(flagged) != 0) {
    warning("Problematic Penguins! Please review: ", paste(flagged, collapse = ", "), call. = FALSE)
  } else {
    rm(flagged, unobsINC, unobsBR)
  }
  
  # 4: remove all unobserved states from data and count success
  breed %>%
    filter(egg_n < 8 & chick_n < 8) %>%
    group_by(nestid) %>% 
    summarize(eggMax = max(egg_n), chxMax = max(chick_n)) %>%
    mutate(ID = nestid,
           HatchSucc = chxMax/eggMax, 
           ObsQuality = as.numeric(eggMax >= chxMax),
           Flagged = as.numeric(nestid %in% flagged)) %>%
    dplyr::select(ID, eggMax, chxMax, HatchSucc, ObsQuality, Flagged)
  
}

  


  
