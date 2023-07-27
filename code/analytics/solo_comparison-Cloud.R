## SOLO NEST COMPARISON


# setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)

source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\fish-tags.R)')
source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\solo_nest.R)')
source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\known_age.R)')

# create an object with solo nest outcomes and historic fishtag nest outcomes
solo_comp <-
  as_tibble(solo_rs) %>%
  dplyr::select(bandnumb = `...1`,
         nuChx = chN,
         crChx = confirmCR) %>% 
  mutate(season = 2122) %>% 
  inner_join(ground_count_avg, by = 'season') %>%
  mutate(bandnumb = as.double(bandnumb),
         nuChx = as.double(nuChx),
         crChx = as.double(crChx),
         anomaly = crChx - avgSuccess,
         type = 'solo') %>% 
  rbind(fishtagComp %>%
          dplyr::select(-c('nuEgg'))) %>%
  mutate(type = factor(type),
         typeNum = as.numeric(type))

solo_ft <- 
  solo_comp %>% 
  dplyr::select(c(nestid = bandnumb, season, type, typeNum, nuChx, crChx, anomaly, type)) %>% 
  mutate(crctChx = anomaly + ground_count_avg$avgSuccess[ground_count_avg$season == 2122])

# create an object with knownage outcomes for solo nests and known-age nests in 2122
solo_ka <-
  as_tibble(solo_rs) %>%
  transmute(nestid = `...1`,
         kaChx = as.character(outcomeKA)) %>% 
  mutate(kaChx = as.numeric(kaChx),
         kaChx = if_else(
           nestid == 6,
           2,
           kaChx),
         type = 'solo') %>% 
  rbind(ka_rs %>%
          dplyr::select(nestid, kaChx, type) %>% 
          mutate(kaChx = if_else(
            is.na(kaChx),
            0,
            kaChx
          ))) %>% 
  mutate(typeNum = if_else(
    type == 'solo', 
    2, # solo is 2
    1 # ka is 1
  ))

# exploration -------------------------------------------------------------

# 2021 adjusted confirmed success values
solo_ft %>%
  group_by(typeNum, season) %>%
  summarize(min = min(crctChx),
            q1 = quantile(crctChx, 0.25),
            mean = mean(crctChx),
            sd = sd(crctChx),
            median = median(crctChx),
            q3 = quantile(crctChx, 0.75),
            max = max(crctChx))

solo_ft %>%
  group_by(season) %>%
  summarize(min = min(crChx),
            q1 = quantile(crChx, 0.25),
            mean = mean(crChx),
            sd = sd(crChx),
            median = median(crChx),
            q3 = quantile(crChx, 0.75),
            max = max(crChx))

# how do anomaly values compare between years
solo_ft %>%
  mutate(season = factor(season)) %>% 
  ggplot(aes(x = season, y = crctChx, color = season)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5) +
  scale_y_continuous()

# let's examine distribution of anomaly values by nest type
solo_ft %>%
  mutate(season = factor(season), typeNum = factor(typeNum)) %>% 
  ggplot(aes(x = crctChx, fill = typeNum)) +
  geom_bar() + 
  facet_grid(typeNum~.)


# known age comparison ----------------------------------------------------

# shapiro-wilk test of normality
with(solo_ka, shapiro.test(kaChx[typeNum == "2"])) # p < 0.05: solos not normal!
with(solo_ka, shapiro.test(kaChx[typeNum == "1"])) # p < 0.05: known-age not normal either!!

# F-test to compare variance between groups
var.test(kaChx ~ typeNum, data = solo_ka) # p = 0.55: no signif. difference in variance

# Wilcoxon two-sample, two-tailed t-test (unequal variance)
with(solo_ka, wilcox.test(kaChx ~ typeNum,
                          alternative = 'two.sided',
                          var.equal = TRUE))
# p = 0.1905
# we cannot reject the null hypothesis that solitary and subcolony nests produce the same number of chicks on average

solo_ka %>% 
  group_by(type) %>% 
  summarise(
    median = median(kaChx),
    mean = mean(kaChx),
    sd = sd(kaChx))

# based on these summaries, solo nests appear to have higher average nest output

# fish-tag comparison --------------------------------------------------------------

# shapiro-wilk test of normality
with(solo_ft, shapiro.test(crctChx[typeNum == "2"])) # p < 0.05: solos not normal!
with(solo_ft, shapiro.test(crctChx[typeNum == "1"])) # p < 0.05: fishes not normal either!!

# F-test to compare variance between groups
var.test(crctChx ~ typeNum, data = solo_ft) # p = 0.65: no signif. difference in variance

# Wilcoxon two-sample, two-tailed t-test (unequal variance)
with(solo_ft, wilcox.test(crctChx ~ typeNum,
                       alternative = 'two.sided',
                       var.equal = TRUE))
# p > 0.001
# the mean success of solo nesters is significantly lower than FT nesters

# kruskall-Wallis analysis of variance

kruskal.test(anomaly ~ season, data = solo_comp)

# post-hoc testing w. mann_whitney u test
source('code/analytics/mann_whitney_u_test.R')

output <- 
  vector('list', length = 4) %>% 
  setNames(c('1617', '1718', '1819', '1920'))

for(i in 1:length(names(output))) {
  names <- as.numeric(names(output))
  x <- names(output)[i]
  output[[x]] <- 
    tibble(
      season = names[i],
      p_value = mann.whitney.test(g1 = 2122,
                                  g2 = names[i],
                                  data = solo_comp),
      corrected_p = mann.whitney.test(g1 = 2122,
                                      g2 = names[i],
                                      data = solo_comp) * length(names)
    )
}

bind_rows(output)

# logit model -------------------------------------------------------------


## confirmed success -------------------------------------------------------

source('code/functions.R')
# use reverse selection to find the best model
# note that here, the highest weight is the variable that should be removed
get_weight(response = 'binaryCR', base = c('rock_yn',
                                           'pebble_distrib_nearby',
                                           'prop_neighbs',
                                           'dist_nearest_subcol_nest_m'), data = solo_rs)
# remove rock-yes or no

get_weight(response = 'binaryCR', base = c('pebble_distrib_nearby',
                                           'prop_neighbs',
                                           'dist_nearest_subcol_nest_m'), data = solo_rs)
# remove distance from subcolony

get_weight(response = 'binaryCR', base = c('pebble_distrib_nearby',
                                           'prop_neighbs'), data = solo_rs)

# remove pebble distribution, proportion of neightbors present alone is the best model

h2.mod <- glm(binaryCR ~ prop_neighbs, data = solo_rs, family = binomial)

summary(h2.mod)

# examine all individual models
h2.rock <- glm(binaryCR ~ rock_yn, data = solo_rs, family = binomial)
h2.pebb <- glm(binaryCR ~ pebble_distrib_nearby, data = solo_rs, family = binomial)
h2.neig <- glm(binaryCR ~ prop_neighbs, data = solo_rs, family = binomial)
h2.dist <- glm(binaryCR ~ dist_nearest_subcol_nest_m, data = solo_rs, family = binomial)

summary(h2.rock)
summary(h2.pebb)
summary(h2.neig)
summary(h2.dist)




#likelihood ratio test compared to null model
with(h2.mod, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



## assumed success ---------------------------------------------------------

# 1
get_weight(response = 'outcomeKA', base = c('rock_yn',
                                           'pebble_distrib_nearby',
                                           'prop_neighbs',
                                           'dist_nearest_subcol_nest_m'), data = solo_rs)
# dist to subcol OUT

# 2
get_weight(response = 'outcomeKA', base = c('rock_yn',
                                            'pebble_distrib_nearby',
                                            'prop_neighbs'), data = solo_rs)
# rock y/n OUT

# 3
get_weight(response = 'outcomeKA', base = c('pebble_distrib_nearby',
                                            'prop_neighbs'), data = solo_rs)
# remove pebbles

h2.mod.ass <- glm(outcomeKA ~ prop_neighbs, data = solo_rs, family = binomial)

h2.ass.rock <- glm(outcomeKA ~ rock_yn, data = solo_rs, family = binomial)
h2.ass.pebb <- glm(outcomeKA ~ pebble_distrib_nearby, data = solo_rs, family = binomial)
h2.ass.dist <- glm(outcomeKA ~ dist_nearest_subcol_nest_m, data = solo_rs, family = binomial)

summary(h2.mod.ass)

summary(h2.ass.rock)
summary(h2.ass.pebb)
summary(h2.ass.dist)
