############### Functions for summarizing the events files #####################

#' Gets total ridehail passengers
#' 
#' @param scenario Events file
#' @param rh_veh_name Pattern to match for ridehail vehicle names
#'
#' @return Total rh passengers
#' 
#' @export
#' 
get_tot_rh_passengers <- function(scenario, rh_veh_name = "rideHailVehicle"){
  
  tot_passengers <- list()
  
  tot_passengers[["enter"]] <- scenario[
    type == "PersonEntersVehicle" &
      str_detect(vehicle, rh_veh_name)
  ] %>% 
    nrow()
  
  tot_passengers[["leave"]] <- scenario[
    type == "PersonLeavesVehicle" &
      str_detect(vehicle, rh_veh_name)
  ] %>% 
    nrow()
  
  tot_passengers[["avg"]] <- mean(
    tot_passengers[["enter"]],
    tot_passengers[["leave"]]
  )
  
  tot_passengers
}



#' Calculate ridehail trips and percentage
#' 
#' @param scenario Events file
#' @param rh_modes List of modes that are ridehail
#' 
#' @return A list of ridehail trip metrics
#' 
#' @export
#' 
get_ridehail_trips <- function(scenario, rh_modes){
  
  trips <- scenario[
    type == "arrival",
    legMode
  ] %>% 
    table() %>% 
    {tibble(
      numTripsRH = sum(.[names(.) %in% rh_modes]),
      numTripsTotal = sum(.)
    )} %>% 
    mutate(RHTripsFrac = numTripsRH / numTripsTotal)
  
  trips
}



#' Calculates ridehail utilization
#' 
#' @param total_riders Total riders by iteration
#' @param rh_fleet RH fleet for the given scenario
#' 
#' @return utilization
#' 
#' @export

get_rh_utilization <- function(total_riders, rh_fleet){
  
  # Utilization as UTA is using the term is 'passengers per
  # vehicle per hour'. So we take the total ridership and
  # divide by the total vehicle-hours of the fleet.
  
  #The number of passengers entering and leaving a RH
  #vehicle is different (though not hugely), so we take
  #an average. There is probably a better solution.
  
  utilization <- total_riders[["avg"]] %>%
    magrittr::divide_by(
      #sum of shift lengths gives total vehicle-hours
      sum(rh_fleet$shiftHours))
  
  utilization
}



#' Calculate average ridehail wait times
#' 
#' @param scenario Events file
#' @param rh_veh_name Pattern to match for ridehail vehicle names
#' 
#' @return Average rh wait time
#'
#' @export
#' 
get_avg_rh_wait_time <- function(scenario, rh_veh_name = "rideHailVehicle"){
  
  scenario_arranged <- scenario[
    type %in% c("ReserveRideHail", "PersonEntersVehicle")
  ][order(person, time)]
  
  wait_time <- list()
  
  wait_time[["times"]] <- scenario_arranged[
  # wait_time <- scenario_arranged[
    ,leadTime := lead(time) - time
  ][type == "ReserveRideHail" & lead(type) == "PersonEntersVehicle" &
      person == lead(person) & str_detect(lead(vehicle), rh_veh_name),
    leadTime] %>%
    magrittr::divide_by(60)

  wait_time[["quantiles"]] <- wait_time[["times"]] %>%
    quantile(na.rm = TRUE,
             probs = c(0, .1, .25, .5, .75, .9, 1))
  
  wait_time
}