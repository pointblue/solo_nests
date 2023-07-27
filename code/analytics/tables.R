
# setup -------------------------------------------------------------------

source(r'(Z:\Informatics\S031\analyses\solo_nests\code\analytics\solo_comparison.R)')

source('code/functions.R')


# extract resight records only from nests in our study
r16 <-
  read_csv(r'(Z:\Informatics\S031\S0311617\croz1617\bandsearch\resight16.csv)') %>% 
  mutate(date = lubridate::mdy(date),
         type = 'Subcolony',
         season = 1617) %>% 
  find_fishtag(., year = 1617)

r17 <-
  read_csv(r'(Z:\Informatics\S031\S0311718\croz1718\bandsearch\resight17.csv)')%>%
  mutate(date = lubridate::mdy(date),
         type = 'Subcolony',
         season = 1718) %>% 
  find_fishtag(., year = 1718)

r18 <- 
  read_csv(r'(Z:\Informatics\S031\S0311819\croz1819\bandsearch\resight18.csv)')%>%
  mutate(date = lubridate::mdy(date),
         type = 'Subcolony',
         season = 1819) %>% 
  find_fishtag(., year = 1819)

r19 <-
  read_csv(r'(Z:\Informatics\S031\S0311920\croz1920\bandsearch\resight19.csv)') %>%
  mutate(date = lubridate::mdy(date),
         type = 'Subcolony',
         season = 1920) %>% 
  find_fishtag(., year = 1920)

r21 <-
  read_csv(r'(Z:\Informatics\S031\S0312122\croz2122\bandsearch\resight21.csv)') %>% 
  mutate(date = lubridate::mdy(date),
         type = 'Subcolony',
         season = 2122) %>%  
  filter(bandnumb %in% ka_outcome$bandnumb) 
  

# table 2 -----------------------------------------------------------------

# detailed table on nest characteristics
# possibly supplementary material


table2 <- 
  fishtagComp %>% 
  transmute(
    season = sapply(season, seas_fy),
    `Nest Type` = 'Subcolony',
    `Active Nests` = nNests,
    `Brood Success` = BrSucc,
    `Creche Success` = CrSucc,
    `Transition Mortality` = transMort) %>% 
  rbind(
    solo_ka %>% 
      mutate(type = if_else(
        type == 'solo',
        'Solitary',
        'Subcolony')) %>% 
      group_by(type) %>% 
      summarize(`Active Nests` = n(),
                `Brood Success` = mean(kaChx)) %>% 
      cbind(
        tibble(
          `Creche Success` = c(All.Success$`Chicks Per Nest`[3], NA))) %>% 
      rename(`Nest Type` = type) %>% 
      mutate(season = 2021,
             `Transition Mortality` = `Brood Success` - `Creche Success`))


# table 3 -----------------------------------------------------------------



  

# calculating median hatch and creche --------------------------------------------------------

  # compile resight records for all penguins in this study
  resight_all <-
    rbind(r16,
          r17,
          r18,
          r19,
          r21) %>% 
    # select columns of interest
    dplyr::select(nestid = bandnumb,
                  date,
                  status,
                  nuegg, 
                  nuch,
                  chick1size,
                  chick2size,
                  type,
                  season,
                  notes) %>% 
    # add solo resight
    rbind(
      (resight_solo %>% 
         dplyr::transmute(nestid,
                          date,
                          status,
                          nuegg = egg_n,
                          nuch = chick_n,
                          chick1size = ch1_size,
                          chick2size = ch2_size,
                          notes, 
                          type = 'Solitary',
                          season = 2021))) 
  
  # find median hatch date
  med_hatch <-
    resight_all %>% 
    group_by(season, type, nestid) %>% 
    filter(grepl('BR', status) & nuch != 9) %>% 
    filter(date == min(date) & nestid != 'solo27') %>% 
    distinct() %>%
    group_by(season, type) %>% 
    arrange(date) %>%
    summarize(
      'Median Hatch' = median(date)) %>% 
    ungroup() %>% 
    mutate(season = sapply(season, seas_fy))
 
  # find median cr date
  seasons <- c(1617, 1718, 1819, 1920)
  
  cr_date <- tibble() 
  
  for (x in seasons) {
    date <- 
      fishtag_outcomes %>% 
      filter(season==x & bandnumb %in% (fishtagComp %>% filter(season == x & crChx >= 1) %>% pull(bandnumb))) %>%
      transmute(season,
                bandnumb,
                result_dt = lubridate::mdy(result_dt)) %>% 
      distinct() %>%
      pull(result_dt) %>% 
      median()
    cr <- tibble(
      season = seas_fy(x),
      type = 'Subcolony',
      `Median Creche` = date)
    cr_date <- 
      rbind(cr, cr_date)
  }
  
  cr_date <-
    cr_date %>% 
    rbind(
      tibble(
        season = 2021, 
        type = 'Subcolony',
        `Median Creche` = ka_outcome %>% 
          filter(grepl('CR', result)) %>% 
          select(bandnumb, result, result_dt) %>%
          mutate(result_dt = lubridate::mdy(result_dt)) %>% 
          pull(result_dt) %>% 
          median(na.rm = T)))
    
  