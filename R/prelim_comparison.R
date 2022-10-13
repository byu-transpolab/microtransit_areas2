####### Comparisons for preliminary analysis #######

compare_scenarios <- function(riders, util, size, wait){
  comparison <- tibble(
    Scenario = names(riders),
    "Fleet Size" = size,
    Ridership = riders %>% unlist(),
    Utilization = util %>% unlist(),
    "Avg. Wait Time (min)" = purrr::map(wait, mean) %>% unlist())
  
  comparison
}



compare_utilization <- function(util, size){
  tibble(Scenario = names(util),
    Utilization = util %>% unlist(),
    "Fleet size" = size)
}



list_wait_times <- function(wait_times){
  comparison <- tibble(
    Scenario = names(unlist(wait_times)) %>% 
      str_remove("\\d+"),
    wait_time = unlist(wait_times)
  ) %>% 
    mutate(Scenario = factor(
      Scenario, levels = c(
        "Existing", "Split", "EX + Davis", "EX + Lehi", "EX + Sandy", "All") %>%
        rev()))

  comparison
}



model_ridership <- function(comparison){
  model <- lm(Ridership ~ `Fleet Size`, comparison)
  
  riders_per_vehicle <- model$coefficients["`Fleet Size`"] %>% 
    round()
  
  riders_per_vehicle
}



compare_rh_fulfillment <- function(fulfillments){
  result <- data.frame(fulfillments) %>% 
    rownames()
  
  table <- as_tibble(fulfillments) %>% 
    mutate(result = result) %>% 
    pivot_longer(-result) %>% 
    pivot_wider(names_from = result) %>% 
    mutate(
      total = fulfilled + replan,
      prop_replan = fulfilled / total
    ) %>% 
    relocate(total, .after = name) %>% 
    `colnames<-`(c(
      "Scenario", "Total", "Fulfilled", "Replanned", "Proporiton Fulfilled"
    ))
  
  table
}