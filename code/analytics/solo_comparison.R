## SOLO NEST COMPARISON


# setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)

source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\fish-tags.R)')
source(r'(Z:\Informatics\S031\analyses\solo_nests\code\cleaning\solo_nest.R)')

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
         type = 'solo') %>% 
  rbind(fishtagComp %>%
          select(-c('nuEgg'))) %>%
  mutate(type = factor(type),
         typeNum = as.numeric(type))

# exploration -------------------------------------------------------------

solo_comp %>%
  group_by(typeNum) %>%
  summarize(min = min(anomaly),
            q1 = quantile(anomaly, 0.25),
            mean = mean(anomaly),
            sd = sd(anomaly),
            median = median(anomaly),
            q3 = quantile(anomaly, 0.75),
            max = max(anomaly))

solo_comp %>%
  group_by(season) %>%
  summarize(min = min(crChx),
            q1 = quantile(crChx, 0.25),
            mean = mean(crChx),
            sd = sd(crChx),
            median = median(crChx),
            q3 = quantile(crChx, 0.75),
            max = max(crChx))

# how do anomaly values compare between years
solo_comp %>%
  mutate(season = factor(season)) %>% 
  ggplot(aes(x = season, y = anomaly, color = season)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5) +
  scale_y_continuous()

# let's examine distribution of anomaly values by nest type
solo_comp %>%
  mutate(season = factor(season), typeNum = factor(typeNum)) %>% 
  ggplot(aes(x = anomaly, fill = typeNum)) +
  geom_bar() + 
  facet_grid(typeNum~.)

# CHICK SIZE
fish_size_cr %>%
  transmute(nestid = as.character(bandnumb),
         size = FinalCHsize,
         ch_bandnumb,
         season,
         type) %>%
  rbind(solo_size_cr %>% 
          mutate(ch_bandnumb = 1)) %>% # pull(size) %>% unique()
  filter(size != 'UND') %>% 
  mutate(season = factor(season),
         size = factor(size, levels = c('OR', 'GF', 'GF1', 'GF2', 'MEL', 'MEL1', 'MEL2', 'PA', 'PA+', 'PA++'))) %>% 
  mutate(z = cut(as.numeric(size), breaks = c(0,1,4,7,9), labels = c('OR', 'GF', 'MEL', 'PA'))) %>% # select(size,z) %>% View()
  ggplot() +
  geom_bar(aes(x = z, fill = season), position = 'dodge') +
  facet_grid(type~.)


# nest success comparison --------------------------------------------------------------

# shapiro-wilk test of normality
with(solo_comp, shapiro.test(anomaly[typeNum == "2"])) # p < 0.05: solos not normal!
with(solo_comp, shapiro.test(anomaly[typeNum == "1"])) # p < 0.05: fishes not normal either!!

# F-test to compare variance between groups
var.test(anomaly ~ typeNum, data = solo_comp) # p = 0.65: no signif. difference in variance

# Wilcoxon two-sample, two-tailed t-test (unequal variance)
with(solo_comp, wilcox.test(anomaly ~ typeNum,
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
test <- 
  solo_rs %>% 
  mutate(neighbor_yn = if_else(prop_neighbs == 0,
                                         0,
                                         1))

h2.mod1 <- glm(binaryCR ~ rock_yn +
                 pebble_distrib_nearby +
                 prop_neighbs +
                 dist_nearest_subcol_nest_m,
               data = solo_rs, family = binomial)
summary(h2.mod1)

# reduce the model to the two variables approaching signif
h2.mod2 <- glm(binaryCR ~ pebble_distrib_nearby +
                 prop_neighbs,
               data = solo_rs, family = binomial)
summary(h2.mod2)

# individual models
h2.mod.1 <- glm(binaryCR ~ pebble_distrib_nearby, data = solo_rs, family = binomial)
h2.mod.2 <- glm(binaryCR ~ prop_neighbs, data = solo_rs, family = binomial)
#h2.mod.2 <- glm(binaryCR ~ neighbor_yn, data = test, family = binomial)
h2.mod.3 <- glm(binaryCR ~ rock_yn, data = solo_rs, family = binomial)

summary(h2.mod.1)
summary(h2.mod.2)
summary(h2.mod.3)

#likelihood ratio test compared to null model
with(h2.mod.3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# size comparison ---------------------------------------------------------

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

# format for chi-square test based on values from prop_ch_size df
M <- as.table(rbind(c(0, 1, 3, 8), c(2, 38, 44, 32))) # table excluding 1718

# add some informative names
dimnames(M) <- list(nest = c("S", "F"),
                      size = c("OR", "GF", "MEL", 'PA'))
# run the chi-squared test
(Xsq <- chisq.test(M))

