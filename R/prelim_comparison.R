####### Comparisons for preliminary analysis #######

compare_scenarios <- function(riders, util, size, wait){
  
  scenarios <- names(riders)
  
  # enters <- riders %>% 
  #   unlist() %>% 
  #   {.[names(.) %>%
  #        str_detect("enter")]}
  # 
  # leaves <- riders %>% 
  #   unlist() %>% 
  #   {.[names(.) %>%
  #        str_detect("leave")]}
  
  comparison <- tibble(
    Scenario = scenarios,
    "Fleet Size" = size,
    Passengers = riders,
    Utilization = util %>% unlist(),
    "Avg. Wait Time (minutes)" = mean(wait) %>% unlist()
  )
  
  comparison
}



compare_utilization <- function(util, size){
  tibble(Scenario = names(util),
    Utilization = util %>% unlist(),
    "Fleet size" = size)
}



list_wait_times <- function(wait_times){
  
  # times <- list()
  # 
  # for (i in names(wait)){
  #   times[[as.character(i)]] <- wait[[i]]$times
  # }

  # comparison <- bind_rows(times)
  comparison <- tibble(
    Scenario = names(unlist(wait_times)) %>% 
      str_remove("\\d+"),
    wait_time = unlist(wait_times)
  )

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