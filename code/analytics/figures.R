
# setup -------------------------------------------------------------------

library(tidyverse)

source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\fish-tags.R)')
source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\solo_nest.R)')

allResight <- read_csv(r'(Z:\Informatics\S031\S0312122\croz2122\bandsearch\allresight_reference_copy.csv)')



# F1: nest map ----------------------------------------------------------------

fishtagComp %>% 
  left_join(allResight %>% 
              select(bandnumb, season, lat, lon) %>%
              mutate(bandnumb = as.double(bandnumb),
                     season = as.double(season)
                     ) %>% # View()
              filter(lat < 0 & lon > 0),
            by = c('season', 'bandnumb')) %>% # View()
  group_by(season, bandnumb) %>%
  summarize(lat = mean(lat), lon = mean(lon), CR = (crChx >= 1), type = 'Subcolony') %>% 
  ungroup() %>% 
    mutate(lat = if_else(is.na(lat),
                         -77.45257,
                         lat),
           lon = if_else(is.na(lon),
                         169.2334,
                         lon)) %>%  #View()
  rbind(solo_rs %>% 
          transmute(season = 2122,
                 bandnumb = `...1`,
                 lat = latitude,
                 lon = longitude,
                 CR = (confirmCR >= 1),
                 type = 'Solitary')) %>% #View()
  ggplot(aes(x = lon, y = lat, color = factor(type))) +
  geom_jitter() + 
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(169.215, 169.246)) +
  scale_y_continuous(limits = c(-77.4625, -77.4475)) +
  scale_color_manual(values = c('Subcolony' = '#FFCC33', 'Solitary' = '#33CCFF'), name = "Nest Type") +
  theme_grey()


# F3: Anomaly values ------------------------------------------------------
solo_comp <-
  as_tibble(solo_rs) %>%
  select(bandnumb = `...1`,
         nuChx = chN,
         crChx = confirmCR) %>% 
  mutate(season = 2122) %>% 
  inner_join(ground_count_avg, by = 'season') %>%
  mutate(bandnumb = as.double(bandnumb),
         nuChx = as.double(nuChx),
         crChx = as.double(crChx),
         anomaly = crChx - avgSuccess,
         type = 'Solitary') %>% 
  rbind(fishtagComp %>%
          select(-c('nuEgg')) %>% 
          mutate(type = 'Subcolony')) %>%
  mutate(type = factor(type, levels = c('Subcolony', 'Solitary'), ordered = TRUE),
         typeNum = as.numeric(type))

solo_comp %>%
  mutate(season = factor(season), typeNum = factor(typeNum)) %>% 
  ggplot(aes(x = anomaly, fill = type)) +
  geom_histogram(color = 'black', bins = 30) + 
  labs(x = 'Anomaly (chicks/nest)', y = 'Frequency') +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c('Subcolony' = '#FFCC33', 'Solitary' = '#33CCFF'),
                    name = "Nest Type") +
  facet_grid(type~.) + 
  theme_grey() +
  theme(legend.position = 'bottom')

# Inset plot of hatch success
solo_outcome <- read_csv("data/solo_outcomes.csv") %>%
  filter(nestid != 'solo27')
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
  mutate(prop = n/sum(n)) %>%
  ungroup() %>% 
  transmute(nuEgg = egg_n,
         nuChx = chick_n,
         n,
         prop = prop * 0.83333,
         type = 'Solitary') %>% 
  filter(nuEgg == 2) %>% 
  rbind(fishtagComp %>%
          mutate(nuChx = if_else(nuChx == 8,
                                 1,
                                 nuChx)) %>%
          group_by(nuEgg, nuChx) %>% 
          summarize(n = n()) %>% 
          mutate(prop = (n/sum(n)) * 0.613,
                 type = 'Subcolony') %>% 
          filter(nuEgg == 2)) %>% 
  ggplot(aes(x = type, y = prop, fill = factor(nuChx, levels = c(2,1,0), ordered = TRUE))) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis(name = 'Hatch Success (n)', discrete = TRUE) +
  labs(x = 'Nest Type', y = 'Proportion') +
  theme_classic() +
  coord_flip() + theme(legend.position = 'bottom') 


# F4: Size distribution ---------------------------------------------------

# faceted plot

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
  mutate(z = cut(as.numeric(size), breaks = c(0,1,4,7,9), labels = c('OR', 'GF', 'MEL', 'PA')),
         type = if_else(type == 1, 'Subcolony', 'Solitary')) %>% # select(size,z) %>% View()
  ggplot() +
  geom_bar(aes(x = z, fill = factor(type, levels = c('Subcolony', 'Solitary'), ordered = TRUE)), color = 'black', position = 'dodge') +
  labs(x = "Size Class", y = 'Frequency') +
  scale_x_discrete(expand = c(0,0.5)) + 
  scale_y_continuous(expand = c(0,0.25)) +
  scale_fill_manual(values = c('Subcolony' = '#FFCC33', 'Solitary' = '#33CCFF'),
                    name = "Nest Type") +
  facet_wrap(~season) + 
  theme_grey()

# stacked plot
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
               values_to = 'prop') %>% 
  mutate(Size = case_when(
    Size == 'A' ~ 'OR', 
    Size == 'B' ~ 'GF', 
    Size == 'C' ~ 'MEL',
    Size == 'D' ~ 'PA'
  )) %>% 
  ggplot() +
  geom_bar(aes(x = factor(season), y = prop, fill = factor(Size, levels = c('PA', 'MEL', 'GF', 'OR'), ordered = TRUE)), stat = 'identity', color = 'black', position = 'stack') +
  labs(x = "Size Class", y = 'Frequency') +
  scale_x_discrete(expand = c(0,0.5)) + 
  scale_y_continuous(expand = c(0,0.025)) +
  scale_fill_manual(values = c('PA' = "#C77CFF", 'MEL' = "#00BFC4", 'GF' = "#7CAE00", 'OR' = "#F8766D"),
                      name = "Size Class") + 
  scale_color_manual(values = c('Subcolony' = '#FFCC33', 'Solitary' = '#33CCFF'),
                    name = NA)
