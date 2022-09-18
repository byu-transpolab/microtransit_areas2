############### Functions for summarizing the events files #####################

#' Filter to ridehail path traversal
#' 
#' @param events Events file
#' @param veh_type Type of vehicle for ridehail
#' @param iters BEAM iterations that were picked
#' 
#' @return Events filtered to ridehail
#' 
#' @export
#'
filter_to_ridehail_pt <- function(events, veh_type = "micro", iters){
  
  rh_events <- list()
  
  for (i in iters){
    rh_events[[as.character(i)]] <- events[[as.character(i)]][
      type == "PathTraversal" & vehicleType == veh_type
    ][order(person, time),
      travelTime := arrivalTime - departureTime
    ][,avgSpeed := length / travelTime
    ][,passengerHours := numPassengers * travelTime / 3600]
  }
  
  rh_events_pt
}



#' Gets total ridehail passengers
#' 
#' @param events Events
#' @param veh_type Type of vehicle for ridehail
#' @param iters BEAM iterations that were picked
#' 
#' @return Total rh passengers
#' 
#' @export
#' 
get_tot_rh_passengers <- function(events, veh_type = "micro", iters){
  
  tot_passengers <- list()
  
  for (i in iters){
    tot_passengers[[as.character(i)]] <- sum(
      events[[as.character(i)]][
        type == "PathTraversal" & vehicleType == veh_type,
        numPassengers]
    )
  }
  
  tot_passengers
}



#' Calculate ridehail trips and percentage
#' 
#' @param events Events file
#' @param rh_modes List of modes that are ridehail
#' @param iters BEAM iterations that were picked
#' 
#' @return A list of ridehail trip metrics
#' 
#' @export
#' 
get_ridehail_trips <- function(events, rh_modes, iters){
  
  trips <- list()
  
  for (i in iters){
    
    trips[[as.character(i)]] <- events[[as.character(i)]][
      type == "arrival",
      legMode
    ] %>%
      table() %>% 
      {tibble(
        numTripsRH = sum(.[names(.) %in% rh_modes]),
        numTripsTotal = sum(.)
      )} %>% 
      mutate(RHTripsFrac = numTripsRH / numTripsTotal)
    
  }
  
  trips
}



#' Calculates ridehail utilization
#' 
#' @param total_riders Total riders by iteration
#' @param rh_fleet RH fleet for the given scenario
#' @param iters
#' 
#' @return utilization
#' 
#' @export

get_rh_utilization <- function(total_riders, rh_fleet, iters){
  
  utilization <- list()
  
  for (i in iters){
    
    utilization[[as.character(i)]] <- total_riders[[as.character(i)]] %>% 
      magrittr::divide_by(
        sum(rh_fleet[["fleet_hours"]]$vehicleHours)
      )
    
  }
  
  utilization
}



#' Calculate average ridehail wait times
#' 
#' @param aranged_events Events arranged by person then time
#' @param iters BEAM iterations picked
#' @param rh_veh_name Pattern to match for ridehail vehicle names
#' 
#' @return Average rh wait time
#'
#' @export
#' 
get_avg_rh_wait_time <- function(arranged_events, iters, rh_veh_name = "rideHailVehicle"){
  
  wait_time <- list()
  
  for (i in iters){
    wait_time[[as.character(i)]] <- list()
    
    wait_time[[as.character(i)]][["times"]] <- arranged_events[[as.character(i)]][
      ,leadTime := lead(time) - time
    ][type == "ReserveRideHail" & lead(type) == "PersonEntersVehicle" &
        person == lead(person) & str_detect(lead(vehicle), rh_veh_name),
      leadTime] %>%
      magrittr::divide_by(60)
    
    wait_time[[as.character(i)]][["quantiles"]] <- wait_time[[
      as.character(i)]][["times"]] %>% 
      quantile(na.rm = TRUE,
               probs = c(0, .1, .25, .5, .75, .9, 1))
    
    # wait_time[[as.character(i)]][["plot"]] <- wait_time[[
    #   as.character(i)]][["times"]] %>%
    #   hist()
  }
  
  wait_time
}



#' Get events for ridehail wait times and arrange them by person then time
#' 
#' @param
#' 
#' @export
#' 
get_arranged_rh_wait_time_events <- function(events, iters){
  
  arranged_events <- list()
  
  for (i in iters){
    arranged_events[[as.character(i)]] <- events[[as.character(i)]][
      type %in% c("ReserveRideHail", "PersonEntersVehicle")
    ][order(person, time)]
  }
  
  arranged_events
}