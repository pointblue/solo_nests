
# setup -------------------------------------------------------------------

library(tidyverse)
library(MASS)
library(effects)
library(jtools)

source('code/cleaning/solo_nest.R')
source('code/analytics/solo_return_rates.R')

solo_obs_2223 <- 
  read_csv('data/solonest_obs_data_entry_2223.csv')

# format ------------------------------------------------------------------

# find the outcome category in 2122
# 1. Non-breed; 2. Failed; 3. Assumed Succ; 4. Conf. Succ

outcome_cats <- 
  solo_rs %>%
  transmute(
    nestid = `...1`,
    outcome = case_when(
     is.na(kaChx) | kaChx == 0 ~ 'FA',
      kaChx >= 1 & confirmCR == 0 ~ 'AS',
      confirmCR >= 1 ~ 'CR')) %>% 
  rbind(
    solo_outcome %>% 
      filter(breeder == 0) %>% 
      separate(nestid, 
               into = c("type", "nestid"), 
               sep = "(?<=[A-Za-z])(?=[0-9])") %>%
      transmute(
        nestid,
        outcome = 'NB')) %>% 
  mutate(
    nestid = as.double(nestid),
    outcome = factor(outcome, levels = c('NB', 'FA', 'AS', 'CR'), ordered = F))

# find the number of neighbors in 2223

neighbs <- 
  solo_obs_2223 %>% 
  # mutate(n_neighbors = as.numeric(n_neighbors)) %>% 
  group_by(nestid) %>% 
  summarize(
    max_neighbs = max(n_neighbors, na.rm = T)) %>% 
  mutate(
    nestid = as.double(nestid),
    max_neighbs = if_else(
    max_neighbs == '1 OR 2',
    2,
    as.numeric(max_neighbs))) %>% 
  filter(!is.na(max_neighbs)) %>% 
  # select only re-occupied nests
  filter(nestid %in% (solo_resight_22 %>% filter(reOcc == 1) %>% pull(nestid))) %>% 
  # bind the maximum number of neighbors observed at each nest in 2021
  left_join(
    (solo_rs %>% 
       dplyr::select(nestid = `...1`, neighbs_21 = max_neighbs))) %>% 
  mutate(
    delta_neighbs = max_neighbs - neighbs_21)

# this will produce two scary warnings,
# 1. caused by cases where no neighbor data was observed for a nest (all NAs)
#2. is caused by the conversion of character inputs to numerics producing new NAs

# both warnings are safe to ignore since they deal with bad data observations
# which are ultimately excluded

# analysis ----------------------------------------------------------------

neighb_mod <-
  neighbs %>% 
  left_join(outcome_cats, by = 'nestid')

n_model2 <- 
  lm(delta_neighbs ~ outcome, data = neighb_mod)

summary(n_model2)

confInt.95 <- confint(n_model2, level = 0.95)
confInt.90 <- confint(n_model2, level = 0.90)

jtools::plot_summs(n_model2)

# convert t-value to p-value
df <- n_model$edf # effective degrees of freedom used in modeling
t <- summary(n_model)$coefficients['AS|CR', "t value"]
# p-value for type (ka(0) vs. solo(1))
p <- 2 * (1 - pt(abs(t), df))


# non-breeders attracted more neighbors than failed nests in the subsequent season
# no other relationships significant

