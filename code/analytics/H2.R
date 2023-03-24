## THIRD SCRIPT RUN ##


# source ------------------------------------------------------------------

source('scripts/wrangling/solo_cleaning.R')

library(data.table)
library(lubridate)
library(stringr)
library(lme4)
library(aod)
library(scales)


# exploration -------------------------------------------------------------

# rocksize - rocksize 2 and 3 will be perfect predictors because there is only 1
ggplot(data = solo_rs) +
  geom_bar(aes(x = rock_yn, fill = outcome_ka), position = "dodge") # rock has little effect, slight success?

# pebbles
ggplot(data = solo_rs) +
  geom_bar(aes(x = pebble_distrib_nearby, fill = outcome_ka), position = "dodge") # more pebbs. assoc. w. fail

# neighbors?
ggplot(data = solo_rs) +
  geom_bar(aes(x = neighbs_yn, fill = outcome_ka), position = "dodge")

ggplot(data = solo_rs) +
  geom_boxplot(aes(x = prop_neighbs, color = outcome_ka), position = "dodge") # neighbors assoc. w. success

# dist_nearest_subcol_nest
ggplot(data = solo_rs) +
  geom_boxplot(aes(x = dist_nearest_subcol_nest_m, color = outcome_ka), position = "dodge") # little app. effect

# area
ggplot(data = solo_rs) +
  geom_bar(aes(x = area, fill = outcome_ka), position = "dodge") # mixed effects and small samples


# model_construction ------------------------------------------------------

# Known-age criteria

h2.mod1 <- glm(outcome_ka ~ rock_yn + pebble_distrib_nearby + prop_neighbs + dist_nearest_subcol_nest_m, data = solo_rs, family = binomial)
summary(h2.mod1)

# reduce the model to the two variables approaching signif

h2.mod2 <- glm(outcome_ka ~ pebble_distrib_nearby + prop_neighbs, data = solo_rs, family = binomial)
summary(h2.mod2)

# individual models

h2.mod.1 <- glm(outcome_ka ~ pebble_distrib_nearby, data = solo_rs, family = binomial)
h2.mod.2 <- glm(outcome_ka ~ prop_neighbs, data = solo_rs, family = binomial)
summary(h2.mod.1)
summary(h2.mod.2)

# confirmed criteria

h2.mod.0 <- glm(cr_confirm ~ rock_yn + pebble_distrib_nearby + prop_neighbs + dist_nearest_subcol_nest_m, data = solo_rs, family = binomial)
summary(h2.mod.0)
h2.mod.0 <- glm(cr_confirm ~ pebble_distrib_nearby + prop_neighbs, data = solo_rs, family = binomial)

with(h2.mod.0, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
