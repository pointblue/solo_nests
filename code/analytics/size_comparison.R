
# setup -------------------------------------------------------------------

library(tidyverse)
library(MASS)

source('code/cleaning/known_age.R')
source('code/cleaning/fish-tags.R')
source('code/cleaning/solo_nest.R')

# size from ka outcome record
size_outcome <- 
  solo_size %>%
  mutate(type = 'Solitary') %>% 
  dplyr::select(-c(creche)) %>%
  rbind((ka_size_outcome %>% 
           transmute(
             nestid = as.character(nestid),
             date,
             chick,
             size,
             type = 'Subcolony'))) %>%
  filter(size >= 'GF1') %>% 
  ungroup() %>% 
  mutate(
    size = case_when(
      size == 'GF1' ~ 1,
      size == 'GF2' ~ 2,
      size == 'MEL1' ~ 3,
      size == 'MEL2' ~ 4,
      size == 'PA' ~ 5,
      size == 'PA+' ~ 6))

# extracting median hatch age ----------------------------------------------------
# find median hatch of solitary nests
raw_solo %>% 
  group_by(nestid) %>% 
  # find all observations with BR status
  filter(grepl('BR', status)) %>% 
  # find the first observation, remove solo 27 which was not sampled regularly
  filter(date == min(date) & nestid != 'solo27') %>% 
  distinct() %>% 
  pull(date) %>% 
  median()

# find median hatch for subcolony nests
initial_ka_obs %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  group_by(bandnumb) %>% 
  # find all observations with BR status
  filter(grepl('BR', status)) %>% 
  # find the first observation, remove solo 27 which was not sampled regularly
  filter(date == min(date)) %>% 
  distinct() %>% 
  pull(date) %>% 
  median()

# size comparison ---------------------------------------------------------
## OUTCOME SIZE
size_outcome %>% 
  group_by(type, size) %>% 
  summarize(count = n(), .groups = 'drop')

# format for chi-square test based on values from prop_ch_size df
M <- as.table(rbind(c(4, 1, 5, 4, 5, 8), c(24, 23, 15, 8, 4, 1))) # table excluding 1718

# add some informative names
dimnames(M) <- list(nest = c("Solo", "Subcol"),
                    size = c('1', "2", "3", '4', '5', '6'))
# run the chi-squared test
(Xsq <- chisq.test(M))


# effect of cr date -------------------------------------------------
model_data <-
  size_outcome %>% 
  # convert size category and nest type into factors for model fitting
  mutate(type = factor(type, levels = c('Subcolony', 'Solitary'), ordered = T),
         size = factor(size, levels = c(1,2,3,4, 5, 6), ordered = T)) %>% 
  # add day of the year variable where 0 is October 1st
  mutate(yday = lubridate::yday(date)) %>%
  mutate(yday = case_when(
    # subcolony nests, median hatch is DEC.24 (yday 358)
    type == 'Subcolony' & yday >= 358 ~ yday - 358,
    type == 'Subcolony' & yday < 358 ~ yday + 7,
    # solitary nests, median hatch is DEC.23 (yday 357)
    type == 'Solitary' & yday >= 357 ~ yday - 357,
    type == 'Solitary' & yday < 357 ~ yday + 8)) %>% 
  # remove two outliers which are probably wrong result dt
  filter(yday <= 300) %>% 
  dplyr::select(c(size, yday, type)) 

size_model <-
  polr(size ~ yday + type, data = model_data)

summary(size_model)


# convert t-value to p-value
df <- size_model$edf # effective degrees of freedom used in modeling
t <- summary(size_model)$coefficients['yday', "t value"]
# p-value for type (ka(0) vs. solo(1))
p <- 2 * (1 - pt(abs(t), df))

# examine the model's predictions
predicted_size <-
  model_data %>% 
  mutate(
    prediction = predict(size_model, newdata = model_data))

# all solos predited to be PA +, all ka predicted GF1

# difference in date ------------------------------------------------------
group0 <- 
  model_data %>% 
  filter(type == 'Subcolony') %>% 
  pull(yday)

group1 <- 
  model_data %>% 
  filter(type == 'Solitary') %>%
  pull(yday)

t.test(group0, group1)
