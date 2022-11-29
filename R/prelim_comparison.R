####### Comparisons for preliminary analysis #######

compare_scenarios <- function(riders, util, size, wait, income){
  comparison <- tibble(
    Scenario = names(riders),
    "Fleet Size" = size,
    Ridership = riders %>% unlist(),
    Utilization = util %>% unlist(),
    "Avg. Wait Time (min)" = purrr::map(wait, mean) %>% unlist()) %>% 
    left_join(income, by = "Scenario") %>% 
    select(-Others) %>% 
    mutate(
      `ODT Users` = `ODT Users` %>% 
        as.integer() %>% 
        format(big.mark = ",") %>%
        {paste0("$", .)}) %>% 
    rename("Median Income of ODT Users" = `ODT Users`)
  
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
        "Existing", "Split", "Existing + Davis", "Existing + Lehi", "Existing + Sandy", "All") %>%
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
      "Scenario", "Total", "Fulfilled", "Replanned", "Prop. Fulfilled"
    ))
  
  table
}


compare_incomes <- function(incomes){
  
  tibble(
    Scenario = names(incomes),
    rh_incomes = purrr::map(
      incomes,
      ~ median(.$rh)) %>% 
      unlist(),
    other_incomes = purrr::map(
      incomes,
      ~ median(.$other)) %>% 
      unlist()
  ) %>% 
    rename("ODT Users" = rh_incomes,
           "Others" = other_incomes)
  
}


# Graph wait times

graph_wait_times <- function(times_list){
  
  plot <- times_list %>% 
    ggplot(aes(x = wait_time, y = Scenario)) +
    geom_violin() +
    stat_summary(fun = mean, geom = "crossbar") +
    labs(x = "Time between ODT request and fulfillment (minutes)",
         y = "Scenario") +
    theme_bw()
  
  ggsave(
    "image/wait_times_graph.png",
    plot,
    width = 6.5,
    height = 4,
    units = "in"
  )
  
  plot
}
