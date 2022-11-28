
# mann_whitney test function -----------------------------------------------------------

mann.whitney.test <- function(g1, g2, data) {
  names <- as.numeric(names(output))
  x <- g1
  y <- g2
  
  # calculate rank sums
  n <- data %>% 
    filter(season == x | season == y) %>%
    nrow()
  
  n1 <- data %>% 
    filter(season == x) %>% 
    nrow()
  
  R1 <- data %>% 
    filter(season == x | season == y) %>%
    mutate(rank = rank(anomaly)) %>% 
    filter(season == x) %>% 
    pull(rank) %>% 
    sum()
  
  n2 <- solo_comp %>% 
    filter(season == y) %>% 
    nrow()
  
  R2 <- solo_comp %>% 
    filter(season == y | season == x) %>%
    mutate(rank = rank(anomaly)) %>% 
    filter(season == y) %>% 
    pull(rank) %>% 
    sum()
  
  u1 <- (n1 * n2) + (n1 * ((n1+1))/2) - R1
  u2 <- (n1 * n2) + (n2 * ((n2+1))/2) - R2
  
  # select the lowest value
  min(u1,u2)
  
  # what is the expected rank sum
  uEX <- ((n1 * n2)/2)
  
  ## calculate the standard error of u with ties
  t <- data %>% 
    filter(season == y | season == x) %>%
    mutate(rank = rank(anomaly)) %>% 
    group_by(rank) %>% 
    summarize(count = n()) %>% 
    filter(count != 1) %>% 
    mutate(t = ((count^3 - count)/12)) %>% 
    pull(t) %>% 
    sum()
  
  uSE <- 
    sqrt((n1 * n2) / (n * (n-1))) * 
    sqrt(((n^3 - n)/12) - t)
  
  ## calculate z and p-values
  z <- 
    (min(u1,u2) - uEX) / uSE
  
  2 * pnorm(abs(z), lower.tail = FALSE)
}

