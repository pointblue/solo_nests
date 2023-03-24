
# find nests with marked chicks from general resight file

find_fishtag <-
  function(resight, year) {
    if(length(unique(resight$season)) != 1) {
      warning('Resight file contains more than one season')}
    
    if(year != (resight$season) %>% unique()) {
      warning('Season argument does not match resight file')}
    
    fishtags <- 
      fishtagComp %>% 
      ungroup() %>% 
      filter(season == year) %>%  
      pull(bandnumb) %>% 
      unique() %>% 
      sort()
    
    fishtags <- 
      resight %>% 
      filter(bandnumb %in% fishtags)
    
    return(fishtags)}