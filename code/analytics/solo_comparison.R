## SOLO NEST COMPARISON


# setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)

source('code/cleaning/fish-tags.R')
source('code/cleaning/solo_nest.R')
source('code/cleaning/known_age.R')

# create an object with solo nest outcomes and historic fishtag nest outcomes
# solo_comp <-
#   as_tibble(solo_rs) %>%
#   dplyr::select(bandnumb = `...1`,
#          nuChx = chN,
#          crChx = confirmCR) %>% 
#   mutate(season = 2122) %>% 
#   inner_join(ground_count_avg, by = 'season') %>%
#   mutate(bandnumb = as.double(bandnumb),
#          nuChx = as.double(nuChx),
#          crChx = as.double(crChx),
#          anomaly = crChx - avgSuccess,
#          type = 'solo') %>% 
#   rbind(fishtagComp %>%
#           dplyr::select(-c('nuEgg'))) %>%
#   mutate(type = factor(type),
#          typeNum = as.numeric(type))
# 
# solo_ft <- 
#   solo_comp %>% 
#   dplyr::select(c(nestid = bandnumb, season, type, typeNum, nuChx, crChx, anomaly, type)) %>% 
#   mutate(crctChx = anomaly + ground_count_avg$avgSuccess[ground_count_avg$season == 2122])

# create an object with brood success for solo nests and subcolony nests in 2122
solo_ka <-
  as_tibble(solo_rs) %>%
  transmute(nestid = `...1`,
         kaChx) %>% 
  mutate(type = 'solo') %>% 
  rbind(ka_rs %>%
          dplyr::select(nestid, kaChx, type)) %>% 
  mutate(typeNum = if_else(
    type == 'solo', 
    2, # solo is 2
    1), # ka is 1
    # replace NAs from nests failed before brood with 0 values
    kaChx = if_else(
      is.na(kaChx),
      0,
      kaChx)) 
  
  

# exploration -------------------------------------------------------------

# 2021 adjusted confirmed success values
# solo_ft %>%
#   group_by(typeNum, season) %>%
#   summarize(min = min(crctChx),
#             q1 = quantile(crctChx, 0.25),
#             mean = mean(crctChx),
#             sd = sd(crctChx),
#             median = median(crctChx),
#             q3 = quantile(crctChx, 0.75),
#             max = max(crctChx))
# 
# solo_ft %>%
#   group_by(season) %>%
#   summarize(min = min(crChx),
#             q1 = quantile(crChx, 0.25),
#             mean = mean(crChx),
#             sd = sd(crChx),
#             median = median(crChx),
#             q3 = quantile(crChx, 0.75),
#             max = max(crChx))
# 
# # how do anomaly values compare between years
# solo_ft %>%
#   mutate(season = factor(season)) %>% 
#   ggplot(aes(x = season, y = crctChx, color = season)) +
#   geom_boxplot() +
#   geom_jitter(alpha = 0.5) +
#   scale_y_continuous()
# 
# # let's examine distribution of anomaly values by nest type
# solo_ft %>%
#   mutate(season = factor(season), typeNum = factor(typeNum)) %>% 
#   ggplot(aes(x = crctChx, fill = typeNum)) +
#   geom_bar() + 
#   facet_grid(typeNum~.)


# brood success comparison ----------------------------------------------------

# shapiro-wilk test of normality
with(solo_ka, shapiro.test(kaChx[typeNum == "2"])) # p < 0.05: solos not normal!
with(solo_ka, shapiro.test(kaChx[typeNum == "1"])) # p < 0.05: known-age not normal either!!

# F-test to compare variance between groups
var.test(kaChx ~ typeNum, data = solo_ka) # p = 0.55: no signif. difference in variance

# Wilcoxon two-sample, two-tailed t-test (unequal variance)
wilcox.result <- with(solo_ka, wilcox.test(kaChx ~ typeNum,
                          alternative = 'two.sided',
                          var.equal = TRUE))
# p = 0.1905
# we cannot reject the null hypothesis that solitary and subcolony nests produce the same number of chicks on average

Brood.Success.2021 <- 
  solo_ka %>% 
  group_by(type) %>% 
  summarise(
    nNests = n(),
    kaTotal = sum(kaChx, na.rm = T),
    median = median(kaChx, na.rm = T),
    mean = mean(kaChx, na.rm = T),
    se = se(kaChx))

# based on these summaries, solo nests appear to have higher average nest output



# creche success comparison -----------------------------------------------

Creche.Success.2021 <- 
  list(
    Avg.Solo.Creche.Success = 
      solo_rs %>% summarize(CrSucc = mean(confirmCR)) %>% pull(CrSucc),
    SE.Solo.Creche.Success = 
      solo_rs %>% summarize(SECrSucc = se(confirmCR)) %>% pull(SECrSucc),
  # minimum estimate if transition mortality
  Max.Estimated.Subcol.Creche.Success = 
    Brood.Success.2021$mean[1] - min(fishtagComp$transMort),
  
  # average transition mortality
  Mean.Estimated.Subcol.Creche.Success = 
    Brood.Success.2021$mean[1] - mean(fishtagComp$transMort),
  
  # maximum estimated of transition mortality
  Min.Estimated.Subcol.Creche.Success = 
    Brood.Success.2021$mean[1] - max(fishtagComp$transMort))

fishtagComp %>% 
  filter(transMort == min(transMort) | transMort == max(transMort))


## clean -------------------------------------------------------------------
# format success data for easy visualization
All.Success <- 
  Brood.Success.2021 %>% 
  dplyr::select(`Nest Type` = type, `Chicks Per Nest` = mean, `Standard Error` = se) %>% 
  mutate(
    `Nest Type` = if_else(
      `Nest Type` == 'ka',
      'Subcolony',
      'Solitary'),
    `Success Metric` = 'Brood Success',
    # is this value observed (obs) or estimated (est)
    `Season` = '2021') %>% 
  rbind(
    tibble(
      `Nest Type` = c('Solitary', 'Subcolony'),
      `Success Metric` = 'Creching Success',
      `Chicks Per Nest` = c(Creche.Success.2021$Avg.Solo.Creche.Success,
                            Creche.Success.2021$Mean.Estimated.Subcol.Creche.Success),
      `Standard Error` = c(Creche.Success.2021$SE.Solo.Creche.Success, NA),
      `Season` = c('2021', 'Historic')))

# document transition mortality
# standard error of transmort for solo nests
SoloTransMort <- 
  solo_rs %>%
  transmute(
    nestid,
    crChx = confirmCR,
    kaChx = if_else(
      is.na(kaChx),
      0,
      kaChx),
    transMort = kaChx - crChx) %>% # pull(transMort) %>% se() %>% 
  summarize(
    mean = mean(transMort),
    se = se(transMort))

SubcolCS.Estimates <-
  tibble(
    `Nest Type` = 'Subcolony',
    season = fishtagComp$season,
    `Success Metric` = 'Creching Success',
    `Chicks Per Nest` = All.Success$`Chicks Per Nest`[1] - fishtagComp$transMort,
    `Season` = 'Historic')

Transition.Mortality <-
  tibble(
    season = c(2021, 2021, fishtagComp$season),
    `Nest Type` = c('Solitary', 'Historic Subcolony', 'Historic Subcolony', 'Historic Subcolony', 'Historic Subcolony', 'Historic Subcolony'),
    transMort = c(-1 * SoloTransMort$mean,
                  -1 * mean(fishtagComp$transMort),
                  -1 *fishtagComp$transMort),
    std.error = c(SoloTransMort$se, NA, NA, NA, NA, NA),
    Mean = c('y', 'y', 'n', 'n', 'n', 'n')) # is this value observed (obs) or estimated (est)

# fish-tag comparison --------------------------------------------------------------

# # shapiro-wilk test of normality
# with(solo_ft, shapiro.test(crctChx[typeNum == "2"])) # p < 0.05: solos not normal!
# with(solo_ft, shapiro.test(crctChx[typeNum == "1"])) # p < 0.05: fishes not normal either!!
# 
# # F-test to compare variance between groups
# var.test(crctChx ~ typeNum, data = solo_ft) # p = 0.65: no signif. difference in variance
# 
# # Wilcoxon two-sample, two-tailed t-test (unequal variance)
# with(solo_ft, wilcox.test(crctChx ~ typeNum,
#                        alternative = 'two.sided',
#                        var.equal = TRUE))
# # p > 0.001
# # the mean success of solo nesters is significantly lower than FT nesters
# 
# # kruskall-Wallis analysis of variance
# 
# kruskal.test(anomaly ~ season, data = solo_comp)


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

size_test <- solo_size %>% 
  mutate(size.num = as.numeric(size))

h.size <- glm(creche ~ size.num, data = size_test, family = binomial)

summary(h2.rock)
summary(h2.pebb)
summary(h2.neig)
summary(h2.dist)
summary(h.size)

test <- 
  tibble(
    size.num = c(1,2,3,4,5,6,7,8,9))

test %>% 
  cbind(
    predictions = predict(h.size, newdata = test)) %>% 
  mutate(predictions = exp(predictions))




#likelihood ratio test compared to null model
with(h2.mod, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



## assumed success ---------------------------------------------------------

solo_rs <-
  solo_rs %>% 
  mutate(
    binaryBR = if_else(
      kaChx >= 1,
      1,
      0))

# 1
get_weight(response = 'binaryBR', base = c('rock_yn',
                                           'pebble_distrib_nearby',
                                           'prop_neighbs',
                                           'dist_nearest_subcol_nest_m'), data = solo_rs)
# rock OUT

# 2
get_weight(response = 'binaryBR', base = c('pebble_distrib_nearby',
                                            'prop_neighbs',
                                           'dist_nearest_subcol_nest_m'), data = solo_rs)
# NULL worst but prop_neighbs close


h2.mod.ass <- glm(binaryBR ~ pebble_distrib_nearby + dist_nearest_subcol_nest_m, data = solo_rs, family = binomial)

h2.ass.rock <- glm(binaryBR ~ rock_yn, data = solo_rs, family = binomial)
h2.ass.pebb <- glm(binaryBR ~ pebble_distrib_nearby, data = solo_rs, family = binomial)
h2.ass.dist <- glm(binaryBR ~ dist_nearest_subcol_nest_m, data = solo_rs, family = binomial)
h2.ass.neig <- glm(binaryBR ~ prop_neighbs, data = solo_rs, family = binomial)
  
summary(h2.mod.ass)

summary(h2.ass.rock)
summary(h2.ass.pebb)
summary(h2.ass.dist)
summary(h2.ass.neig)


# predicted success -------------------------------------------------------
test <-
  with(
  solo_rs,
  data.frame(
    dist_nearest_subcol_nest_m = seq(3,20, 0.25),
    pebble_distrib_nearby = factor(0)))

test$predicted <- predict(h2.mod.ass, newdata = test, type = 'response')

