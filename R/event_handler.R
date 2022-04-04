#' Selects and adds columns of interest
#' 
#' @param events_raw Events to be formatted
#' @param cols Vector of colnames to keep
format_events <- function(events_raw, cols){
  events <- events_raw %>% 
    select(all_of(cols)) %>% 
    mutate(
      travelTime = arrivalTime - departureTime,
      avgSpeed = length / travelTime
    ) %>% 
    {data.table::as.data.table(.)[order(person,time)]} %>%
    as_tibble()
  
  events
}


#' join tibbles
all_join <- function(events_list, func, join_col, col_1_name, ...){
  
  events <- map(events_list, func, ...)
  
  full <- events[[1]]
  
  for(i in 2:length(events_list)){
    full <- full %>% 
      full_join(events[[i]], by = join_col)
  }
  
  full %>% 
    `colnames<-`(c(col_1_name, names(events_list)))
}



#### Functions ####

#' mode choice
mode_choice <- function(events){
  events %>% 
    filter(type == "ModeChoice") %>% 
    group_by(mode) %>% 
    summarize(n = n())
}


#' rh passengers
rh_pass <- function(events){
  rhPassengers <- events %>%
    filter(type == "PathTraversal",
           vehicleType == "micro") %>%
    select(numPassengers) %>%
    table() %>% as_tibble() %>%
    `colnames<-`(c("numPassengers", "n"))
  
  rhPassengers
}


#' rh times
rh_times <- function(events){
  times <- events %>%
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
    group_by(rhReserveOutcome)
  
  times %>% 
    summarise(median = median(rhReserveTime) / 60)
}


#' rh travel times
rh_travel_times <- function(events){
  events %>% 
    filter(!is.na(travelTime),
           type == "PathTraversal",
           str_detect(vehicle, "rideHail")) %>% 
    mutate(travelTime = (travelTime) / 60) %>% 
    separate(vehicle, c("vehicle_number", "area"), sep = "@") %>% 
    select(area, travelTime)
}


#' rh utilization
rh_utilization <- function(events, rh_info){
  rhUtil <- events %>%
    filter(type == "PathTraversal",
           vehicleType == "micro") %>% 
    separate(vehicle, c("vehicle_number", "area"), sep = "@") %>% 
    select(area, numPassengers) %>% 
    table() %>% as_tibble() %>% 
    left_join(rh_info, by = c("area" = "Area")) %>% 
    transmute(area,
              utilization = as.numeric(numPassengers) * n
              / fleetSize / operating_hours) %>% 
    group_by(area) %>% 
    summarise(utilization = sum(utilization))
  
  rhUtil
}
  
