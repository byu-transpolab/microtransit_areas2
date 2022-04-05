#' Selects and adds columns of interest
#' 
#' @param events_raw Events to be formatted
#' @param cols Vector of colnames to keep
read_events <- function(events_raw, cols){
  events <- events_raw %>% 
    read_csv() %>% 
    select(all_of(cols)) %>% 
    mutate(
      travelTime = arrivalTime - departureTime,
      avgSpeed = length / travelTime
    ) %>% 
    {data.table::as.data.table(.)[order(person,time)]} %>%
    as_tibble()
  
  events
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


#' format rh info
format_rh_info <- function(rh_raw){
  rh_info <- rh_raw %>% 
    read_csv() %>% 
    mutate(shift_start = str_replace(shifts, "\\{(\\d+):\\d+\\}", "\\1") %>% 
             as.numeric(),
           shift_end = str_replace(shifts,"\\{\\d+:(\\d+)\\}", "\\1") %>% 
             as.numeric(),
           operating_hours = (shift_end - shift_start) / 3600) %>% 
    select(Area, fleetSize, operating_hours)
  
  rh_info
}


#' find connections from rh to transit
rh_transit_connections <- function(events){
  test1 <- events %>% 
    filter(
      type %in% c("PersonEntersVehicle",
                  "PersonLeavesVehicle"),
      str_detect(vehicle, "(?:gtfs|rideHail)")
    ) %>% 
    arrange(person, time) %>% 
    filter((str_detect(vehicle, "rideHail") &
           str_detect(lead(vehicle), "gtfs")) |
             (str_detect(lag(vehicle), "rideHail") &
                str_detect(vehicle, "gtfs")) |
             (str_detect(vehicle, "rideHail") &
                str_detect(lead(vehicle), "gtfs")) |
             (str_detect(lag(vehicle), "rideHail") &
                str_detect(vehicle, "gtfs")))
  
  
  arranged <- events %>% 
    arrange(person, time)
  filtered <- arranged %>% 
    filter((type == "PersonLeavesVehicle" &
              lead(type) == "PersonEntersVehicle") | 
             (lag(type) == "PersonLeavesVehicle" &
                type == "PersonEntersVehicle"))
  test2 <- filtered %>% 
    filter((str_detect(vehicle, "rideHail") &
              str_detect(lead(vehicle), "gtfs")) |
             (str_detect(lag(vehicle), "rideHail") &
                str_detect(vehicle, "gtfs")))
    
}