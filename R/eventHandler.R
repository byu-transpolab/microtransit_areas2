#### Event Handler Functions ####

read_events <- function(eventsCSV){
  eventCols <- read_csv("data/eventCols.csv", col_names = F)
  coltypes <- set_names(pull(eventCols, 2), pull(eventCols ,1))
  
  fullEvents <- read_csv(eventsCSV, col_types = coltypes)
  
  eventCols <- c("person",
                 "time",
                 "type",
                 "mode",
                 "legMode",
                 "vehicleType",
                 "vehicle",
                 "arrivalTime",
                 "departureTime",
                 "departTime",
                 "length",
                 "numPassengers",
                 "actType",
                 "personalVehicleAvailable")

  events <- fullEvents %>% select(all_of(eventCols)) %>% 
    mutate(travelTime = arrivalTime - departureTime,
           avgSpeed = length / travelTime
           ) %>%
    {as.data.table(.)[order(person,time)]} %>%
    as_tibble()
}


compare_scenarios <- function(scen = list(),
                              veh_type = "micro",
                              nVehicles = rep.int(12, length(scen)),
                              shifts = rep.int(72900, length(scen))
                              ){
  
  rhPassengers <- scen %>%
    map(filter,
        type == "PathTraversal",
        vehicleType == veh_type)
  
  rhModes <- c("ride_hail",
               "ride_hail_pooled",
               "ride_hail_transit")
  
  
  
  totRiders <- rhPassengers %>% 
    map(select, numPassengers) %>% 
    map(sum) %>% 
    unlist()
  
  avgOccTravelTime <- rhPassengers %>% 
    map(filter, numPassengers > 0) %>% 
    map(select, travelTime) %>% 
    map(colMeans) %>% 
    unlist() %>% 
    divide_by(60)
  
  utilization <- totRiders / nVehicles / (shifts/3600)

  avgWaitTimes <- scen %>% 
    map(rh_wait_times) %>% 
    map(mean) %>%
    unlist() %>% 
    divide_by(60)
  
  numRHTrips <- scen %>% 
    map(filter,
        type == "arrival",
        legMode %in% rhModes
        ) %>% 
    map(nrow) %>% 
    unlist()
  
  numTotTrips <- scen %>% 
    map(filter, type == "arrival") %>% 
    map(nrow) %>% 
    unlist()
  
  pctRHModeChoice <- numRHTrips / numTotTrips * 100
  
  ########################################
  
  #Make table
  
  tibble(
    "Scenario" = names(scen),
    "Weekday Ridership" = totRiders,
    "Ride Hail Trips" = numRHTrips,
    "Percent of RH Trips" = pctRHModeChoice,
    "Average Wait Time (minutes)" = avgWaitTimes,
    # "Average Occupied Travel Time (minutes)" = avgOccTravelTime, ##Not well defined
    "Utilization" = utilization
  )
  
}

rh_wait_times <- function(arranged_events){
  arranged_events %>% 
    mutate(leadTime = lead(time) - time) %>% 
    filter(type == "ReserveRideHail",
           lead(type) == "PersonEntersVehicle",
           person == lead(person)
           ) %>% 
    select(leadTime) %>% 
    unlist() %>% 
    unname()
}
