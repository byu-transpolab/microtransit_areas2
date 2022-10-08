######## Compare UTA On Demand to BEAM ##############

#' Pivots UTA On Demand info to have months as columns
#' 
#' @param UTAOD UTA On Demand info
#'
#' @export
#'
pivot_uta <- function(UTAOD){
  
  ridership <- mean(UTAOD$`Avg wkday ridership`)
  util <- mean(UTAOD$Utilization)
  wait <- mean(UTAOD$`Avg wait time (minutes)`)
  
  UTAOD %>% 
    pivot_longer(-Month) %>% 
    pivot_wider(names_from = Month) %>% 
    rename(" " = name,
           January = JAN,
           February = FEB,
           March = MAR) %>% 
    add_column(Mean = c(ridership, util, wait))
}


compare_existing <- function(UTA, riders, util, wait){
  
  comparison <- tibble(
    c("UTA Observed Data", "BEAM 'Pilot' Scenario"),
    c(UTA$Mean[1], riders),
    c(UTA$Mean[2], util),
    c(UTA$Mean[3], mean(wait))
  )
  
  colnames(comparison) <- c(
    " ", "Ridership", "Utilization", "Avg. wait time (min)")
  
  comparison
  
}


make_uta_table <- function(UTAOD, months){
  
  UTA_good <- read_csv(UTAOD) %>% 
    filter(Month %in% months)
  
  UTA <- read_csv(UTAOD) %>% 
    add_row(
      Month = "Average",
      `Avg wkday ridership` = mean(.$`Avg wkday ridership`),
      Utilization = mean(.$Utilization),
      `Avg wait time (minutes)` = mean(.$`Avg wait time (minutes)`)
    ) %>% 
    add_row(
      Month = "Average JAN--MAR",
      `Avg wkday ridership` = mean(UTA_good$`Avg wkday ridership`),
      Utilization = mean(UTA_good$Utilization),
      `Avg wait time (minutes)` = mean(UTA_good$`Avg wait time (minutes)`)
    )
  
  UTA
  
}