
# setup -------------------------------------------------------------------

initial_ka_obs <- read_csv("data/ka_resight21.csv")
source('code/analytics/hatch_success_function.R')


# solo nests --------------------------------------------------------------

solo_hatch <- hatch_success(data = raw_obs, 
                            bandnumb = 'nestid',
                            eggs = 'egg_n', 
                            chicks = 'chick_n', 
                            status = 'status')
# checked flagged penguins
## amend the only case where my call is different than the function
### Function suggests no chx but Megan observed at least one (8)
solo_hatch$chxMax[solo_hatch$ID == 42] <- 1


# known age nests ---------------------------------------------------------

hatch_success(data = initial_ka_obs)
filter(
  bandgap != 48221
)
[.$bandnumb == 48391] <- 2 'egg'


