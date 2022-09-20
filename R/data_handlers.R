#### Functions for reading and handling data ###################################

#' Get and process ridehail fleet
#' 
#' @param fleet_file RH fleet file
#' 
#' @return RH fleet with extra info
#' 
#' @export
#' 
read_ridehail_fleet <- function(fleet_file){
  
  rh_fleet <- read_csv(fleet_file) %>% 
    separate(shifts, c(NA, "startTime", "endTime", NA), remove = FALSE) %>% 
    mutate(startTime = as.integer(startTime),
           endTime = as.integer(endTime),
           shiftHours = (endTime - startTime) / 3600)

  rh_fleet
}


get_fleet_sizes <- function(fleets){
  map(fleets, nrow) %>% 
    unlist()
}


get_event_cols <- function(cols_list){
  filtered <- read_csv(cols_list) %>% 
    filter(include == "y")
  
  cols <- filtered$type
  names(cols) <- filtered$col_name
  
  cols
}