
# mode --------------------------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# find fishtag ------------------------------------------------------------

# find nests with marked chicks from general resight file

find_fishtag <-
  function(resight, year) {
    if(length(unique(resight$season)) != 1) {
      warning('Resight file contains more than one season')}
    
    if(year != (resight$season) %>% unique()) {
      warning('Season argument does not match resight file')}
    
    fishtags <- 
      fishtagAll %>% 
      ungroup() %>% 
      filter(season == year) %>%  
      pull(bandnumb) %>% 
      unique() %>% 
      sort()
    
    fishtags <- 
      resight %>% 
      filter(bandnumb %in% fishtags)
    
    return(fishtags)}

# season year transform ---------------------------------------------------


# transform season code to season year
seas_fy <- function(s) {
  #  s here is like "1920" for 2019 - 2020; returns 4 digit year
  y1 <- substring(s, 1, 2)
  if (as.numeric(y1) > 90) {
    sfy <- paste0("19", y1)
  }
  else {
    sfy <- paste0("20", y1)
  }
  #RETURN season_fullyr
  return(as.numeric(sfy))
}



# get weights -------------------------------------------------------------


# create a function to calculate AIC weights for different models
# this permutation will be for reverse selection of variables
get_weight <- 
  # function will accept inputs where response will be the name of the response variable
  # base is a vector of established predictors,
  # and data is the dataframe from which to construct the models
  function(response = NULL, base = NULL, data = NULL) {
    # help the user (me) get their shit together
    if(is.null(response)) {
      stop('Response variable is missing from call')
    }
    if(is.null(data)) {
      stop('Data source is missing from call')
    }
    # create a repository
    models <- vector(
      mode = 'list',
      length = length(base)) %>% 
      set_names(unique(base))
    # create the base formula
    form <- 
      paste(response, '~', paste0(base,
                                  collapse = '',
                                  sep = '+'),
            sep = '')
    
    # find AIC for each test model
    for(x in names(models)) {
      
      form.new <-
        str_remove(string = form, pattern = paste(x, '[+]', sep = '')) 
      
      form.new <- 
        if(str_ends(string = form.new, pattern = '[+]')){
          str_sub(form.new,0,-2)}
      
      m1 <- 
        glm(formula = as.formula(form.new),
            data = data,
            family = binomial)
      
      models[x] <- 
        AIC(m1)}
    
    # now create a reference model with only the base variables
    form.new <- 
      if(str_ends(string = form, pattern = '[+]')){
        str_sub(form,0,-2)}
    
    m2 <- 
      glm(formula = as.formula(form.new),
          data = data,
          family = binomial)
    models[['Base']] <- 
      AIC(m2)
    
    # calculate AIC weights
    vec_AIC <- unlist(models)
    dAIC <- vec_AIC - min(vec_AIC)
    AICw <- exp(-dAIC/2) / sum(exp(-dAIC/2))
    return(AICw)
  }


# standard error ----------------------------------------------------------

se <- 
  function(x) {
    sd(x) / sqrt(length(x))
  }
