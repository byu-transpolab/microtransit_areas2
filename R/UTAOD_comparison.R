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
    c("UTA Observed Data", "BEAM 'Existing' Scenario"),
    c(UTA$Mean[1], riders$avg),
    c(UTA$Mean[2], util),
    c(UTA$Mean[3], wait$quantiles["50%"])
  )
  
  colnames(comparison) <- c(
    " ", "Ridership", "Utilization", "Avg. wait time (min)")
  
  comparison
  
}