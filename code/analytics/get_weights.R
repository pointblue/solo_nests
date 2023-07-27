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
