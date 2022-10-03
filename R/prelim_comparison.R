####### Comparisons for preliminary analysis #######

compare_riders <- function(riders){
  
  scenarios <- names(riders)
  
  enters <- riders %>% 
    unlist() %>% 
    {.[names(.) %>%
         str_detect("enter")]}
  
  leaves <- riders %>% 
    unlist() %>% 
    {.[names(.) %>%
         str_detect("leave")]}
  
  comparison <- tibble(
    Scenario = scenarios,
    Entering = enters,
    Leaving = leaves
  )
  
  comparison
}



compare_utilization <- function(util, size){
  tibble(Scenario = names(util),
    Utilization = util %>% unlist(),
    "Fleet size" = size)
}



compare_wait_times <- function(wait){
  
  times <- list()

  for (i in names(wait)){
    times[[as.character(i)]] <- wait[[i]]$times
  }

  comparison <- bind_rows(times)

  comparison
   
  # comparison <- tibble(names(wait[[1]]$times))
  # 
  # for (i in wait){
  #   comparison <- comparison %>% 
  #     add_column(i$quantiles, .name_repair = "minimal")
  # }
  # 
  # colnames(comparison) <- c(
  #   "Quantile", names(wait))
  # 
  # comparison <- comparison %>% 
  #   pivot_longer(-Quantile, names_to = "Scenario") %>% 
  #   pivot_wider(names_from = Quantile, values_from = value)
  #
  # comparison
}