#' Compute distribution of reserve vs replanning times
#' 
#' @param events_list
#' 
make_reserve_times <- function(events_list){
  rh <- future_lapply(events_list, function(events){
    events %>% 
      arrange(person, time) %>%
      mutate(
        rhReserveTime = ifelse(
          type == "ReserveRideHail" & person == lead(person),
          lead(time) - time,
          NA
        ),
        rhReserveOutcome = ifelse(
          type == "ReserveRideHail" & person == lead(person),
          lead(type),
          NA
        )
      ) %>%
      filter(!is.na(rhReserveTime)) %>%
      select(person, rhReserveTime, rhReserveOutcome)
  }) %>% 
    bind_rows(.id = "scenario")
  
  rh
}



make_utilization <- function(events_list){
  ut <- future_lapply(events_list, function(events) {
    events %>%
      filter(type %in% c("PersonEntersVehicle", "PersonLeavesVehicle")) %>%
      filter(grepl("rideHail", vehicle)) %>%
      select(person, vehicle, time, type) %>%
      group_by(vehicle) %>%
      arrange(time, .by_group = TRUE)  %>%
      mutate(
        passengers = passenger_volume(type),
        elapsed = lead(time) - time
      ) %>%
      separate(vehicle, c("vehicle_number", "area"), sep = "@") 
  }) %>%
    bind_rows(.id = "scenario")
  
  ut
}

#' Calculate the number of passengers in the ridehail at each enter / leave event
#' @param type Sorted vector of person enter / leave vehicle events
passenger_volume <- function(type){
  nevents <- length(type) 
  volume <- rep(0, nevents)
  
  for(i in 2:nevents){
    if(type[i] == "PersonEntersVehicle") {
      volume[i] <- volume[i - 1] + 1
    } else if(type[i] == "PersonLeavesVehicle") {
      volume[i] <- volume[i - 1] - 1
    }
  }
  
  return(volume)
}



make_ridership <- function(events_list){
  ri <- future_lapply(events_list, function(events) {
    events  %>%
      filter(type %in% c("PersonEntersVehicle")) %>%
      filter(grepl("rideHail", vehicle)) %>%
      separate(vehicle, c("vehicle_number", "area"), sep = "@")  %>%
      select(person:type)
  }) %>%
    bind_rows(.id = "scenario")
  
  ri
}



make_firstlast <- function(events_list){
  fl <- future_lapply(events_list, function(events) {
    
    # people who leave a ridehail and enter a transit vehicle
    first <- events %>% 
      filter(
        type %in% c("PersonEntersVehicle",
                    "PersonLeavesVehicle"),
        str_detect(vehicle, "rideHail") | str_detect(vehicle, "gtfs")
      ) %>% 
      arrange(person, time) %>% 
      select(person:type) %>% 
      filter(type == "PersonLeavesVehicle" & lead(type) == "PersonEntersVehicle",
             str_detect(vehicle, "rideHail") & str_detect(lead(vehicle), "gtfs"),
             person == lead(person)) %>%
      separate(vehicle, c("vehicle_number", "area"), sep = "@")  %>%
      mutate(transfer = "rh to transit")
    
    # people who leave a transit vehicle and enter a ridehail 
    last <-  events %>% 
      filter(
        type %in% c("PersonEntersVehicle",
                    "PersonLeavesVehicle"),
        str_detect(vehicle, "rideHail") | str_detect(vehicle, "gtfs")
      ) %>% 
      arrange(person, time) %>% 
      select(person:type) %>% 
      filter(lag(type) == "PersonLeavesVehicle" & type == "PersonEntersVehicle",
             str_detect(lag(vehicle), "gtfs") & str_detect(vehicle, "rideHail"),
             person == lag(person)) %>%
      separate(vehicle, c("vehicle_number", "area"), sep = "@")  %>%
      mutate(
        transfer = "transit to rh",
        
      ) 
      
    
    bind_rows(first, last)
  }) %>%
    bind_rows(.id = "scenario")
  
  fl
  
}
