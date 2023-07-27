
# setup -------------------------------------------------------------------

library(tidyverse)
library(MASS)

source('code/cleaning/known_age.R')

source('code/cleaning/solo_nest.R')

# size from ka resight record
size_resight <-
  solo_size %>% 
  mutate(
    `cr?` = if_else(
      size >= 'GF2',
      1,
      0)) %>% 
  rbind(
    (ka_size_resight %>% 
       mutate(nestid = as.character(nestid),
              `cr?` = if_else(
                size >= 'GF2',
                1,
                0),
              creche = 0))) %>% 
  ungroup()

# size from ka outcome record
size_outcome <- 
  solo_size %>% 
  filter(size >= 'GF2') %>% 
  dplyr::select(-c(creche)) %>%
  rbind((ka_size_outcome %>% 
           rename(status = result, notes = note) %>% 
       mutate(nestid = as.character(nestid)))) %>%
  ungroup() 

# exploration -------------------------------------------------------------

# plot resight size frequency
size_resight %>% 
  group_by(type, size) %>% 
  summarize(count = n(), .groups = 'drop') %>%
  left_join(
    (size_resight %>% 
      group_by(type) %>% 
      summarize(total = n(), .groups = 'drop'))) %>% 
  mutate(perc = count/total) %>% 
ggplot() +
  geom_bar(aes(x = size, y = perc), stat = 'Identity') +
  facet_grid(type~.)


# plot outcome size frequency
size_outcome %>% 
  group_by(type, size) %>% 
  summarize(count = n(), .groups = 'drop') %>%
  left_join(
    (size_outcome %>% 
       group_by(type) %>% 
       summarize(total = n(), .groups = 'drop'))) %>% 
  mutate(perc = (count/total)*100,
         type = if_else(
           type == 'ka',
           'Subcolony',
           'Solitary'),
         size = case_when(
           size == 'PA' ~ 'PA1',
           size == 'PA+' ~ 'PA2',
           size == 'GF2' ~ 'GF2',
           size == 'MEL1' ~ 'MEL1',
           size == 'MEL2' ~ 'MEL2')) %>% # View()
  ggplot() +
  geom_bar(aes(x = size, y = perc, fill = type), color = 'black', stat = 'Identity') +
  scale_fill_manual(values = c('Subcolony' = '#33CCFF', 'Solitary' = '#FFCC33'), name = "Nest Type") +
  scale_x_discrete(expand = c(0.1,0.1)) +
  scale_y_continuous(expand = c(0,0.1))+
  labs(y = 'Percentage (%)', x = 'Chick Size Class') +
  facet_grid(type~.) +
  theme_classic() + 
  theme(legend.position = 'bottom')

# compare creche date between types
# center dates around October 1st as start of Adelie penguin breeding season
size_outcome %>% 
  mutate( type = if_else(
    type == 'solo',
    'Solitary',
    'Subcolony'),
    # type = factor(type, levels = c('Subcolony', 'Solitary'), ordered = T),
    yday = lubridate::yday(date),
    yday = if_else(
      yday >= 358,
      (yday - 358),
      yday + 7)) %>%
  mutate() %>% # group_by(type) %>% summarize(min = min(yday), q1 = quantile(yday, 0.25), mean = mean(yday), median = median(yday), q3 = quantile(yday, 0.75), max = max(yday))
  filter(yday <= 300)  %>% 
  ggplot() +
  geom_boxplot(aes(x = type, y = yday, fill = type)) +
  scale_y_continuous(position = 'right') +
  scale_fill_manual(values = c('Solitary' = '#FFCC33', 'Subcolony' = '#33CCFF'), name = "Nest Type") +
  labs(x = 'Nest Type', y = 'Crèche Date (Days since Median Hatch)') +
  guides(fill = 'none') +
  # coord_flip() +
  theme_classic()


# extracting chick age ----------------------------------------------------




# size comparison ---------------------------------------------------------
## RESIGHT SIZE
size_resight %>% 
  group_by(type, size) %>% 
  summarize(count = n(), .groups = 'drop')

# format for chi-square test based on values from prop_ch_size df
R <- as.table(rbind(c(7, 8, 4, 7, 1, 5, 4, 5, 8), c(7, 29, 27, 30, 24, 16, 7, 3, 3))) # table excluding 1718

# add some informative names
dimnames(R) <- list(nest = c("Solo", "Subcol"),
                    size = c("LEM", 'OR1', 'OR2', 'GF1', "GF2", "MEL1", "MEL2", 'PA', 'PA+'))
# run the chi-squared test
(Xsq <- chisq.test(R))

## OUTCOME SIZE
size_outcome %>% 
  group_by(type, size) %>% 
  summarize(count = n(), .groups = 'drop')

# format for chi-square test based on values from prop_ch_size df
M <- as.table(rbind(c(1,5, 4, 5, 8), c(23, 15, 8, 4, 1))) # table excluding 1718

# add some informative names
dimnames(M) <- list(nest = c("Solo", "Subcol"),
                    size = c("GF2", "MEL1", "MEL2", 'PA', 'PA+'))
# run the chi-squared test
(Xsq <- chisq.test(M))


# effect of cr date -------------------------------------------------
model_data <-
  size_outcome %>% 
  mutate(type = factor(type, levels = c('ka', 'solo'), ordered = T)) %>% 
  # add day of the year variable where 0 is October 1st
  mutate(yday = lubridate::yday(date)) %>%
  mutate(yday = if_else(
    yday >= 358,
    (yday - 358),
    yday + 7)) %>% 
  # remove two outliers which are probably wrong result dt
  filter(yday <= 300) %>% 
  dplyr::select(c(size, yday, type)) 

size_model <-
  polr(size ~ yday + type, data = model_data)

summary(size_model)


# convert t-value to p-value
df <- size_model$edf # effective degrees of freedom used in modeling
t <- summary(size_model)$coefficients['type.L', "t value"]
# p-value for type (ka(0) vs. solo(1))
p <- 2 * (1 - pt(abs(t), df))

# examine the model's predictions
predicted_size <-
  model_data %>% 
  mutate(
    prediction = predict(size_model, newdata = model_data))

# all solos predited to be PA +, all ka predicted GF2


# difference in date ------------------------------------------------------
group0 <- 
  model_data %>% 
  filter(type == 'ka') %>% 
  pull(yday)

group1 <- 
  model_data %>% 
  filter(type == 'solo') %>%
  pull(yday)

t.test(group0, group1)


# fish-tag chick sizes ----------------------------------------------------

fish_size_cr %>%
  # reformat data to combine w. solo nest sizes
  transmute(nestid = as.character(bandnumb),
            size = FinalCHsize,
            ch_bandnumb,
            season,
            type) %>%
  rbind(solo_size_cr %>% 
          mutate(ch_bandnumb = 1)) %>% # pull(size) %>% unique() 
  
  # factorize season and chSize for easy analysis
  filter(size != 'UND') %>% 
  mutate(season = factor(season),
         size = factor(size, levels = c('OR', 'GF', 'GF1', 'GF2', 'MEL', 'MEL1', 'MEL2', 'PA', 'PA+', 'PA++'))) %>% 
  
  # aggregate fruit sizes into OR, GF, MEL, PA
  mutate(z = cut(as.numeric(size), breaks = c(0,1,4,7,9), labels = c('OR', 'GF', 'MEL', 'PA'))) %>% # select(size,z) %>% View()
  
  # exclude potentially anomalous year
  filter(season != 1718) %>%
  
  # count the number and proportion of chx in each size class
  group_by(z, type) %>%
  summarise(n = n()) %>%
  distinct() %>%
  ungroup()

