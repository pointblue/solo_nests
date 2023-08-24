## SOLO NEST COMPARISON


# setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)

source('code/cleaning/fish-tags.R')
source('code/cleaning/solo_nest.R')
source('code/cleaning/known_age.R')

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
# p = 0.9823
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
    `Data` = 'Observed') %>% 
  rbind(
    tibble(
      `Nest Type` = c('Solitary', 'Subcolony'),
      `Success Metric` = 'Creching Success',
      `Chicks Per Nest` = c(Creche.Success.2021$Avg.Solo.Creche.Success,
                            Creche.Success.2021$Mean.Estimated.Subcol.Creche.Success),
      `Standard Error` = c(Creche.Success.2021$SE.Solo.Creche.Success, NA),
      `Data` = c('Observed', 'Expected')))

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

# values of expected creching success for 2021 based on historic trans. mortality
SubcolCS.Estimates <-
  tibble(
    `Nest Type` = 'Subcolony',
    season = fishtagComp$season,
    `Success Metric` = 'Creching Success',
    `Chicks Per Nest` = All.Success$`Chicks Per Nest`[1] - fishtagComp$transMort,
    `Data` = 'Expected')

Transition.Mortality <-
  tibble(
    season = c(2021, 2021, fishtagComp$season),
    `Nest Type` = c('Solitary', 'Historic Subcolony', 'Historic Subcolony', 'Historic Subcolony', 'Historic Subcolony', 'Historic Subcolony'),
    transMort = c(1 * SoloTransMort$mean,
                  1 * mean(fishtagComp$transMort),
                  1 *fishtagComp$transMort),
    std.error = c(SoloTransMort$se, NA, NA, NA, NA, NA),
    Mean = c('y', 'y', 'n', 'n', 'n', 'n')) # is this value observed (obs) or estimated (est)
